{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Plagiarism.Config where

import Data.Maybe
import Data.Aeson
import qualified Data.Text as T

import Lichen.Config
import Lichen.Languages

data Config = Config { configFile :: FilePath
                     , outputDir :: FilePath
                     , concatDir :: FilePath
                     , highlightDir :: FilePath
                     , reportDir :: FilePath
                     , language :: Language
                     , topMatches :: Int
                     , semester :: Maybe T.Text
                     , course :: Maybe T.Text
                     , assignment :: Maybe T.Text
                     }
instance FromJSON Config where
        parseJSON = withObject "config_plagiarism" $ \o -> do
            configFile <- fromMaybe (configFile defaultConfig) <$> o .:? "config_file"
            outputDir <- fromMaybe (outputDir defaultConfig) <$> o .:? "output_dir"
            concatDir <- fromMaybe (concatDir defaultConfig) <$> o .:? "concat_dir"
            highlightDir <- fromMaybe (highlightDir defaultConfig) <$> o .:? "highlight_dir"
            reportDir <- fromMaybe (reportDir defaultConfig) <$> o .:? "report_dir"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            topMatches <- fromMaybe (topMatches defaultConfig) <$> o .:? "top_matches"
            semester <- fromMaybe (semester defaultConfig) <$> o .:? "semester"
            course <- fromMaybe (course defaultConfig) <$> o .:? "course"
            assignment <- fromMaybe (assignment defaultConfig) <$> o .:? "assignment"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { configFile = ".lichenrc"
                       , outputDir = "plagiarism"
                       , concatDir = "concatenated"
                       , highlightDir = "highlighted"
                       , reportDir = "report"
                       , language = langDummy
                       , topMatches = 100
                       , semester = Nothing
                       , course = Nothing
                       , assignment = Nothing
                       }

type Plagiarism = Configured Config
