{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Config.Diagnostics where

import Data.Maybe
import Data.Aeson

import Lichen.Config
import Lichen.Config.Languages

data Config = Config
            { dataDir :: FilePath
            , language :: Language
            , mode :: String
            , tag :: Maybe String
            , sourceFiles :: [FilePath]
            }
instance FromJSON Config where
        parseJSON = withObject "config_diagnostics" $ \o -> do
            dataDir <- fromMaybe (dataDir defaultConfig) <$> o .:? "data_dir"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            mode <- fromMaybe (mode defaultConfig) <$> o .:? "mode"
            tag <- fromMaybe (tag defaultConfig) <$> o .:? "tag"
            sourceFiles <- fromMaybe (sourceFiles defaultConfig) <$> o .:? "source_files"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { dataDir = ".lichen"
                       , language = langDummy
                       , mode = ""
                       , tag = Nothing
                       , sourceFiles = []
                       }

type Diagnostics = Configured Config
