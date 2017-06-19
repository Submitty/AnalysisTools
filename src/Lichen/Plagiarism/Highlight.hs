module Lichen.Plagiarism.Highlight where

import System.Directory

import Lichen.Config

highlight :: FilePath -> IO ()
highlight p = do
        srcPath <- (++) <$> pure "plagiarism_data/concatenated" <*> canonicalizePath p
        dstPath <- (++) <$> pure "plagiarism_data/highlighted" <*> canonicalizePath p
        createDirectoryIfMissing True dstPath
        toHighlightSrcDst <- fmap (\x -> (srcPath ++ "/" ++ x, dstPath ++ "/" ++ x)) <$> listDirectory srcPath
        mapM_ (uncurry copyFile) toHighlightSrcDst
