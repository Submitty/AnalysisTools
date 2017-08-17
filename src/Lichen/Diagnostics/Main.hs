{-# LANGUAGE OverloadedStrings #-}

module Lichen.Diagnostics.Main where

import System.Directory
import System.FilePath

import Data.List
import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T.L.E
import qualified Data.Text.Lazy.IO as T.L.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Util
import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Diagnostics
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Compare
import Lichen.Plagiarism.Walk
import Lichen.Plagiarism.Winnow
import Lichen.Plagiarism.AssignmentSettings
import qualified Lichen.Config.Plagiarism as Plagiarism

compareTag :: String -> [(Fingerprints, String)] -> [(Double, (Fingerprints, String), (Fingerprints, String))]
compareTag t prints = case ours of Just x -> curry compareFingerprints x <$> prints
                                   Nothing -> []
    where tagged [] = Nothing
          tagged (x@(_, p):ps) | p == t = Just x | otherwise = tagged ps
          ours = tagged prints

diagnosticsToken :: Language -> FilePath -> Erring Pair
diagnosticsToken (Language _ l _ _ _) p = do
        src <- liftIO $ BS.readFile p
        tokens <- l p src
        return $ T.pack p .= toJSON tokens

cleanMatchTag :: String -> [(Double, (Fingerprints, String), (Fingerprints, String))] -> [(Double, (Fingerprints, String), (Fingerprints, String))]
cleanMatchTag _ [] = []
cleanMatchTag t ((m, a@(_, x), b@(_, y)):xs) | t == x = (m, a, b):cleanMatchTag t xs
                                             | t == y = (m, b, a):cleanMatchTag t xs
                                             | otherwise = cleanMatchTag t xs

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> showDefault <> value (dataDir dc) <> help "Directory to store internal data")
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> strOption (long "mode" <> short 'm' <> metavar "MODE" <> value (mode dc) <> help "Method of diagnostic information")
               <*> optional (strOption (long "tag" <> short 't' <> metavar "TAG" <> help "Tag to query in plagiarism mode"))
               <*> many (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain ic = do
        iopts <- liftIO . execParser $ opts ic
        mcsrc <- readSafe BS.L.readFile Nothing (dataDir iopts </> "config_diagnostics.json")
        options <- case mcsrc of Just csrc -> do
                                     c <- case eitherDecode csrc of Left e -> (printError . JSONDecodingError $ T.pack e) >> pure ic
                                                                    Right t -> pure t
                                     liftIO . execParser $ opts c
                                 Nothing -> pure iopts
        flip runConfigured options $ case mode options of
            "lex" -> do
                config <- ask
                ps <- liftIO . mapM canonicalizePath $ sourceFiles config
                os <- lift $ mapM (diagnosticsToken (language config)) ps
                liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 . encode $ object os
            "plagiarism" -> case headSafe (sourceFiles options) of
                Nothing -> throwError $ InvocationError "No directory specified"
                Just sd -> liftIO . flip runConfigured (Plagiarism.defaultConfig { Plagiarism.dataDir = dataDir options, Plagiarism.language = language options, Plagiarism.sourceDir = Just sd }) $ do
                    config <- ask
                    p <- case Plagiarism.sourceDir config of Just d -> return d; Nothing -> throwError $ InvocationError "No directory specified"
                    dir <- liftIO $ canonicalizePath p
                    concatenateActive dir
                    prints <- fingerprintDir (Plagiarism.language config) (Plagiarism.dataDir config </> Plagiarism.concatDir config ++ dir)
                    case tag options of
                        Just ttag -> do
                            let d = fmap (\(x, (_, _), (_, t)) -> object [ "tag" .= t, "match" .= x ]) . sortBy (\(x, _, _) (y, _, _) -> compare y x) $ compareTag ttag prints
                            st <- getStudentTime (dir </> ttag </> "user_assignment_settings.json")
                            liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 . encode $ object [ "tag" .= ttag, "time" .= st, "matches" .= d ]
                        Nothing -> do
                            let cmp = sortBy (\(x, _, _) (y, _, _) -> compare y x) $ crossCompare prints []
                                tags = snd <$> prints
                            objects <- mapM (\ttag -> do
                                            st <- getStudentTime (dir </> ttag </> "user_assignment_settings.json")
                                            return $ object ["tag" .= ttag, "time" .= st, "matches" .= fmap (\(x, (_, _), (_, t)) -> object [ "tag" .= t, "match" .= x ]) (cleanMatchTag ttag cmp)]) tags
                            liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 $ encode objects
            _ -> return ()
    where opts c = info (helper <*> parseOptions c) (fullDesc <> progDesc "Output diagnostic information about source files" <> header "diagnostics - output assorted information about source code")
