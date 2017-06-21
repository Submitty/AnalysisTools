module Lichen.Plagiarism.Highlight where

import System.Directory
import System.FilePath

import Control.Applicative
import Control.Monad.Reader
import Control.Arrow ((&&&))

import Lichen.Util
import Lichen.Config.Plagiarism

highlight :: FilePath -> FilePath -> Plagiarism ()
highlight base p = do
        config <- ask
        srcPath <- liftIO $ liftA2 (++) (pure $ base </> dataDir config </> concatDir config) $ canonicalizePath p
        dstPath <- liftIO $ liftA2 (++) (pure $ base </> dataDir config </> highlightDir config) $ canonicalizePath p
        liftIO $ removeIfDoesntExist dstPath >> createDirectoryIfMissing True dstPath
        toHighlightSrcDst <- liftIO $ fmap ((</>) srcPath &&& (</>) dstPath) <$> listDirectory srcPath
        liftIO $ mapM_ (uncurry copyFile) toHighlightSrcDst
