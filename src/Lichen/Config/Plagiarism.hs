{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Config.Plagiarism where

import Data.Maybe
import Data.Aeson
import qualified Data.Text as T

import Lichen.Config
import Lichen.Config.Languages

data Config = Config
            { dataDir :: FilePath
            , concatDir :: FilePath
            , highlightDir :: FilePath
            , reportDir :: FilePath
            , reportTitle :: T.Text
            , language :: Language
            , topMatches :: Int
            , sourceDir :: Maybe FilePath
            , pastDirs :: [FilePath]
            }
instance FromJSON Config where
        parseJSON = withObject "config_plagiarism" $ \o -> do
            dataDir <- fromMaybe (dataDir defaultConfig) <$> o .:? "data_dir"
            concatDir <- fromMaybe (concatDir defaultConfig) <$> o .:? "concat_dir"
            highlightDir <- fromMaybe (highlightDir defaultConfig) <$> o .:? "highlight_dir"
            reportDir <- fromMaybe (reportDir defaultConfig) <$> o .:? "report_dir"
            reportTitle <- fromMaybe (reportTitle defaultConfig) <$> o .:? "report_title"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            topMatches <- fromMaybe (topMatches defaultConfig) <$> o .:? "top_matches"
            sourceDir <- fromMaybe (sourceDir defaultConfig) <$> o .:? "source_dir"
            pastDirs <- fromMaybe (pastDirs defaultConfig) <$> o .:? "past_dirs"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { dataDir = ".lichen"
                       , concatDir = "concatenated"
                       , highlightDir = "highlighted"
                       , reportDir = "report"
                       , reportTitle = "Plagiarism Detection"
                       , language = langDummy
                       , topMatches = 100
                       , sourceDir = Nothing
                       , pastDirs = []
                       }

type Plagiarism = Configured Config
