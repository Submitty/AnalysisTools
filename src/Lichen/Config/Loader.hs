{-# LANGUAGE GADTs #-}

module Lichen.Config.Loader where

import System.IO
import System.Environment
import System.Directory
import System.FilePath

import Data.Hashable

import Control.Monad.Reader

import qualified Config.Dyre as Dyre

import Lichen.Config
import Lichen.Config.Languages
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Highlight
import Lichen.Plagiarism.Walk
import Lichen.Plagiarism.Compare

plagiarismMain :: PlagiarismConfig -> IO ()
plagiarismMain = runReaderT $ do
    config <- ask
    [p] <- liftIO getArgs
    base <- liftIO $ getEnv "LICHEN_CWD"
    dir <- liftIO $ canonicalizePath (base </> p)
    concatenate base dir
    highlight base dir
    prints <- fingerprintDir (language config) (base </> dataDir config </> concatDir config ++ dir)
    liftIO . print $ crossCompare prints

runPlagiarism = Dyre.wrapMain $ Dyre.defaultParams
    { Dyre.projectName = "lichen-plagiarism"
    , Dyre.realMain = plagiarismMain
    , Dyre.statusOut = hPutStrLn stderr
    , Dyre.configDir = Just $ getEnv "LICHEN_CWD"
    , Dyre.cacheDir = Just $ (</>) <$> getEnv "LICHEN_CWD" <*> pure ".lichen/cache"
    }
