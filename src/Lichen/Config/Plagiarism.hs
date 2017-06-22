module Lichen.Config.Plagiarism where

import Lichen.Config
import Lichen.Config.Languages

data Config = Config
            { dataDir :: FilePath
            , concatDir :: FilePath
            , highlightDir :: FilePath
            , sourceDir :: Maybe FilePath
            , language :: Language
            }

defaultConfig :: Config
defaultConfig = Config { dataDir = ".lichen"
                       , concatDir = "concatenated"
                       , highlightDir = "highlighted"
                       , sourceDir = Nothing
                       , language = langDummy
                       }

type Plagiarism = Configured Config
