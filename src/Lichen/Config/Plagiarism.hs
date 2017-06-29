{-# LANGUAGE OverloadedStrings #-}

module Lichen.Config.Plagiarism where

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
