module Lichen.Plagiarism.Highlight where

import System.Directory

import Lichen.Config

-- Given a path p, transform the files in
-- plagiarism_data/concatenated/<dir> into
-- plagiarism_data/highlighted/<dir> where dir is the absolute path of p.
-- This transformation should preserve file names, and is intended to be
-- used to perform syntax highlighting to facilitate better rendering.
highlight :: FilePath -> IO ()
highlight p = do
        srcPath <- (++) <$> pure "plagiarism_data/concatenated" <*> canonicalizePath p
        dstPath <- (++) <$> pure "plagiarism_data/highlighted" <*> canonicalizePath p
        createDirectoryIfMissing True dstPath
        toHighlightSrcDst <- fmap (\x -> (srcPath ++ "/" ++ x, dstPath ++ "/" ++ x)) <$> listDirectory srcPath
        mapM_ (uncurry copyFile) toHighlightSrcDst
