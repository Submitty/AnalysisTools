{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Main where

import System.Directory
import System.FilePath

import Data.Semigroup ((<>))
import qualified Data.Text.Lazy.Encoding as T.E
import qualified Data.Text.Lazy.IO as T.IO

import Control.Monad.Reader
import Control.Monad.Except

import Text.Blaze.Html.Renderer.Utf8

import Options.Applicative

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Highlight
import Lichen.Plagiarism.Walk
import Lichen.Plagiarism.Compare
import Lichen.Plagiarism.Render.Index

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> strOption (long "datadir" <> short 'd' <> metavar "DIR" <> showDefault <> value (dataDir dc) <> help "Directory to store internal data")
               <*> strOption (long "concatdir" <> short 'c' <> metavar "DIR" <> showDefault <> value (concatDir dc) <> help "Subdirectory of data directory storing concatenated student code")
               <*> strOption (long "highlightdir" <> short 'i' <> metavar "DIR" <> showDefault <> value (highlightDir dc) <> help "Subdirectory of data directory storing syntax-highlighted student code")
               <*> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> optional (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain c = do
        options <- liftIO $ execParser opts
        flip runConfigured options $ do
            config <- ask
            p <- case sourceDir config of Just d -> return d; Nothing -> throwError $ InvocationError "No directory specified"
            dir <- liftIO $ canonicalizePath p
            concatenate dir
            highlight dir
            prints <- fingerprintDir (language config) (dataDir config </> concatDir config ++ dir)
            liftIO . T.IO.putStrLn . T.E.decodeUtf8 . renderHtml $ table (crossCompare prints)
    where opts = info (helper <*> parseOptions c) (fullDesc <> progDesc "Run plagiarism detection" <> header "lichen-plagiarism - plagiarism detection")
