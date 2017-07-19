{-# LANGUAGE OverloadedStrings #-}

module Lichen.Diagnostics.Main where

import System.Directory
import System.FilePath

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T.L.E
import qualified Data.Text.Lazy.IO as T.L.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L

import Control.Monad.Reader

import Options.Applicative

import Lichen.Util
import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Diagnostics

diagnosticsToken :: Language -> FilePath -> Erring Pair
diagnosticsToken (Language _ l _ _ _) p = do
        src <- liftIO $ BS.readFile p
        tokens <- l p src
        return $ T.pack p .= toJSON tokens

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "data-dir" <> short 'd' <> metavar "DIR" <> showDefault <> value (dataDir dc) <> help "Directory to store internal data")
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
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
        flip runConfigured options $ do
            config <- ask
            ps <- liftIO . mapM canonicalizePath $ sourceFiles config
            os <- lift $ mapM (diagnosticsToken (language config)) ps
            liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 . encode $ object os
    where opts c = info (helper <*> parseOptions c) (fullDesc <> progDesc "Output diagnostic information about source files" <> header "diagnostics - output assorted information about source code")
