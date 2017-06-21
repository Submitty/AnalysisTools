{-# LANGUAGE OverloadedStrings #-}

module Lichen.CountToken.Main where

import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Data.Hashable
import Data.Semigroup ((<>))
import qualified Data.ByteString as BS

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import qualified Config.Dyre as Dyre

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.CountToken
import Lichen.Lexer

countToken :: Language -> FilePath -> String -> Erring Int
countToken (Language _ lex _ readTok) p t = do
        src <- liftIO $ BS.readFile p
        tokens <- lex p src
        return . length . filter (hash (readTok t) ==) . fmap hash $ tokens

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> optional (argument str (metavar "TOKEN"))
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> optional (argument str (metavar "SOURCE"))
    where maybeDefault :: Maybe a -> Maybe a -> Maybe a
          maybeDefault x (Just y) = Nothing
          maybeDefault x Nothing = x

realMain :: Config -> IO ()
realMain c = do
        options <- liftIO $ execParser opts
        flip runConfigured options $ do
            config <- ask
            base <- liftIO $ getEnv "LICHEN_CWD"
            tok <- case token config of Just t -> return t; Nothing -> throwError $ InvocationError "No token specified"
            relsrc <- case sourceFile config of Just s -> return s; Nothing -> throwError $ InvocationError "No source file specified"
            src <- liftIO $ canonicalizePath (base </> relsrc)
            occurences <- lift $ countToken (language config) src tok
            liftIO $ print occurences
    where opts = info (helper <*> parseOptions c) (fullDesc <> progDesc "Count occurences of a specific lexical token" <> header "lichen-count-token - token counting")

run = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "lichen-count-token"
    , Dyre.realMain = realMain
    , Dyre.statusOut = hPutStrLn stderr
    , Dyre.configDir = Just $ getEnv "LICHEN_CWD"
    , Dyre.cacheDir = Just $ (</>) <$> getEnv "LICHEN_CWD" <*> pure ".lichen/cache"
    }
