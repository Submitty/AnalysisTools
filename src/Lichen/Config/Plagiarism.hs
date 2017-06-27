module Lichen.Config.Plagiarism where

import Lichen.Config
import Lichen.Config.Languages

data Config = Config
            { dataDir :: FilePath
            , concatDir :: FilePath
            , highlightDir :: FilePath
            , language :: Language
            , sourceDir :: Maybe FilePath
            }

defaultConfig :: Config
defaultConfig = Config { dataDir = ".lichen"
                       , concatDir = "concatenated"
                       , highlightDir = "highlighted"
                       , language = langDummy
                       , sourceDir = Nothing
                       }

type Plagiarism = Configured Config
