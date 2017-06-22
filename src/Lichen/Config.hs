module Lichen.Config where

import Control.Monad.Reader
import Control.Monad.Except

import Lichen.Error

type Configured c = ReaderT c Erring

runConfigured :: Configured c () -> c -> IO ()
runConfigured m c = do
        result <- runExceptT $ runReaderT m c
        case result of Left e -> printError e; Right _ -> return ()
