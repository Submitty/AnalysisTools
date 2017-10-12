{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Diagnostics.Config where

import Data.Maybe
import Data.Aeson

import Lichen.Config
import Lichen.Languages

data Config = Config
            { configFile :: FilePath
            , language :: Language
            , sourceFiles :: [FilePath]
            }
instance FromJSON Config where
        parseJSON = withObject "config_diagnostics" $ \o -> do
            configFile <- fromMaybe (configFile defaultConfig) <$> o .:? "config_file"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            sourceFiles <- fromMaybe (sourceFiles defaultConfig) <$> o .:? "source_files"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { configFile = ".lichenrc"
                       , language = langDummy
                       , sourceFiles = []
                       }

type Diagnostics = Configured Config
