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
            , sourceDir :: Maybe FilePath
            }
instance FromJSON Config where
        parseJSON = withObject "config_plagiarism" $ \o -> do
            dataDir <- fromMaybe (dataDir defaultConfig) <$> o .:? "data_dir"
            concatDir <- fromMaybe (concatDir defaultConfig) <$> o .:? "concat_dir"
            highlightDir <- fromMaybe (highlightDir defaultConfig) <$> o .:? "highlight_dir"
            reportDir <- fromMaybe (reportDir defaultConfig) <$> o .:? "report_dir"
            reportTitle <- fromMaybe (reportTitle defaultConfig) <$> o .:? "report_tkitle"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            sourceDir <- fromMaybe (sourceDir defaultConfig) <$> o .:? "source_dir"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { dataDir = ".lichen"
                       , concatDir = "concatenated"
                       , highlightDir = "highlighted"
                       , reportDir = "report"
                       , reportTitle = "Plagiarism Detection"
                       , language = langDummy
                       , sourceDir = Nothing
                       }

type Plagiarism = Configured Config
