{-# LANGUAGE OverloadedStrings #-}

module Lichen.Diagnostics.Main where

import System.Directory

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as T.L.E
import qualified Data.Text.Lazy.IO as T.L.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.L

import Text.Blaze.Html.Renderer.Utf8
import qualified Text.Blaze.Html5 as H

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Util
import Lichen.Error
import Lichen.Config
import Lichen.Languages
import Lichen.Diagnostics.Config
import Lichen.Diagnostics.Render

diagnosticsJSON :: Language -> FilePath -> Erring Pair
diagnosticsJSON (Language _ _ _ lr pr) p = do
        src <- liftIO $ BS.readFile p
        tokens <- lr p src
        nodes <- pr p src
        return $ T.pack p .= object [ "tokens" .= toJSON tokens
                                    , "nodes" .= toJSON nodes
                                    ]

diagnosticsHTML :: Language -> FilePath -> Erring H.Html
diagnosticsHTML (Language _ _ _ lr _) p = do
        src <- liftIO $ BS.readFile p
        tokens <- lr p src
        tokensPage <- liftIO $ renderTokens p tokens
        return $ renderPage tokensPage

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "config-file" <> short 'c' <> metavar "PATH" <> showDefault <> value (configFile dc) <> help "Configuration file")
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> strOption (long "output-format" <> short 'f' <> metavar "FORMAT" <> showDefault <> value (outputFormat dc) <> help "Output format")
               <*> many (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain ic = do
        iopts <- liftIO . execParser $ opts ic
        mcsrc <- readSafe BS.L.readFile Nothing $ configFile iopts
        options <- case mcsrc of
            Just csrc -> do
                c <- case eitherDecode csrc of
                    Left e -> (printError . JSONDecodingError $ T.pack e) >> pure ic
                    Right t -> pure t
                liftIO . execParser $ opts c
            Nothing -> pure iopts
        flip runConfigured options $ do
            config <- ask
            if null $ sourceFiles config
            then throwError $ InvocationError "No source files provided"
            else do
                ps <- liftIO . mapM canonicalizePath $ sourceFiles config
                case outputFormat config of
                    "html" -> do
                        page <- lift . diagnosticsHTML (language config) $ head ps
                        liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 $ renderHtml page
                    "dense" -> do
                        os <- lift $ mapM (diagnosticsJSON (language config)) ps
                        liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 . encode $ object os
                    _ -> do
                        os <- lift $ mapM (diagnosticsJSON (language config)) ps
                        liftIO . T.L.IO.putStrLn . T.L.E.decodeUtf8 . encodePretty $ object os
    where opts c = info (helper <*> parseOptions c)
                        (fullDesc <> progDesc "Output diagnostic information about source files" <> header "diagnostics - output assorted information about source code")
