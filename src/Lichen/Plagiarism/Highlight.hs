module Lichen.Plagiarism.Highlight where

import System.Directory
import System.FilePath

import Control.Monad.Reader
import Control.Arrow ((&&&))

import Lichen.Config

highlight :: FilePath -> FilePath -> Plagiarism ()
highlight base p = do
        config <- ask
        srcPath <- liftIO $ (++) <$> pure (base </> dataDir config </> concatDir config) <*> canonicalizePath p
        dstPath <- liftIO $ (++) <$> pure (base </> dataDir config </> highlightDir config) <*> canonicalizePath p
        liftIO $ removeDirectoryRecursive dstPath >> createDirectoryIfMissing True dstPath
        toHighlightSrcDst <- liftIO $ fmap ((</>) srcPath &&& (</>) dstPath) <$> listDirectory srcPath
        liftIO $ mapM_ (uncurry copyFile) toHighlightSrcDst
