module Lichen.Config.CountToken where

import Control.Monad.Reader
import Control.Monad.Except

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Lexer

data Config = Config
            { token :: Maybe String
            , language :: Language
            , sourceFile :: Maybe FilePath
            }

defaultConfig :: Config
defaultConfig = Config { token = Nothing
                       , language = langDummy
                       , sourceFile = Nothing
                       }

type CountToken = Configured Config
