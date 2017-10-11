{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Count.Config where

import Data.Maybe
import Data.Aeson

import Lichen.Config
import Lichen.Languages
import Lichen.Count.Counters

data Config = Config
            { dataDir :: FilePath
            , language :: Language
            , counter :: Counter
            , toCount :: Maybe String
            , sourceFiles :: [FilePath]
            }
instance FromJSON Config where
        parseJSON = withObject "config_count" $ \o -> do
            dataDir <- fromMaybe (dataDir defaultConfig) <$> o .:? "data_dir"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            counter <- fromMaybe (counter defaultConfig) <$> o .:? "counter"
            toCount <- fromMaybe (toCount defaultConfig) <$> o .:? "to_count"
            sourceFiles <- fromMaybe (sourceFiles defaultConfig) <$> o .:? "source_files"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { dataDir = ".lichen"
                       , language = langDummy
                       , counter = counterDummy
                       , toCount = Nothing
                       , sourceFiles = []
                       }

type Count = Configured Config
