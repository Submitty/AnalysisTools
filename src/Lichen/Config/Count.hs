{-# LANGUAGE OverloadedStrings #-}

module Lichen.Config.Count where

import Control.Monad.Except

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages

type Counter = Language -> String -> FilePath -> Erring Integer

counterDummy :: Counter
counterDummy _ _ _ = throwError $ InvocationError "Invalid counting method specified"

data Config = Config
            { language :: Language
            , method :: Counter
            , toCount :: Maybe String
            , sourceFiles :: [FilePath]
            }

defaultConfig :: Config
defaultConfig = Config { language = langDummy
                       , method = counterDummy
                       , toCount = Nothing
                       , sourceFiles = []
                       }

type Count = Configured Config
