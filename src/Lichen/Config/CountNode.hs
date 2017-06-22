module Lichen.Config.CountNode where

import Lichen.Config
import Lichen.Config.Languages

data Config = Config
            { language :: Language
            , node :: Maybe String
            , sourceFile :: Maybe FilePath
            }

defaultConfig :: Config
defaultConfig = Config { language = langDummy
                       , node = Nothing
                       , sourceFile = Nothing
                       }

type CountNode = Configured Config
