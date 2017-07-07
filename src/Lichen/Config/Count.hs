{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lichen.Config.Count where

import GHC.Generics

import Data.Aeson

import Lichen.Config
import Lichen.Config.Languages
import Lichen.Count.Counters

data Config = Config
            { language :: Language
            , counter :: Counter
            , toCount :: Maybe String
            , sourceFiles :: [FilePath]
            } deriving Generic
instance FromJSON Config where

defaultConfig :: Config
defaultConfig = Config { language = langDummy
                       , counter = counterDummy
                       , toCount = Nothing
                       , sourceFiles = []
                       }

type Count = Configured Config
