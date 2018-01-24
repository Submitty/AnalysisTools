{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Count.Config where

import Data.Maybe
import Data.Aeson

import Lichen.Config
import Lichen.Languages
import Lichen.Count.Counters

data Config = Config { configFile :: FilePath
                     , language :: Language
                     , counter :: Counter
                     , toCount :: Maybe String
                     , sourceFiles :: [FilePath]
                     }
instance FromJSON Config where
        parseJSON = withObject "config_count" $ \o -> do
            configFile <- fromMaybe (configFile defaultConfig) <$> o .:? "config_file"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            counter <- fromMaybe (counter defaultConfig) <$> o .:? "counter"
            toCount <- fromMaybe (toCount defaultConfig) <$> o .:? "to_count"
            sourceFiles <- fromMaybe (sourceFiles defaultConfig) <$> o .:? "source_files"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { configFile = ".lichenrc"
                       , language = langDummy
                       , counter = counterDummy
                       , toCount = Nothing
                       , sourceFiles = []
                       }

type Count = Configured Config
