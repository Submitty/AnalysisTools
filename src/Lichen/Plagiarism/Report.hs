module Lichen.Plagiarism.Report where

import System.Directory
import System.FilePath

import Data.List
import qualified Data.ByteString.Lazy as BS

import Control.Applicative
import Control.Monad.Reader

import Text.Blaze.Html.Renderer.Utf8

import Lichen.Util
import Lichen.Plagiarism.Config
import Lichen.Plagiarism.Winnow
import Lichen.Plagiarism.Compare
import Lichen.Plagiarism.Render
import Lichen.Plagiarism.Render.Index
import Lichen.Plagiarism.Render.Compare

report :: (Show a, Eq a) => FilePath -> [(Fingerprints, a)] -> [(Fingerprints, a)] -> Plagiarism ()
report p prints past = do
        config <- ask
        dstPath <- liftIO $ liftA2 (++) (pure $ outputDir config </> reportDir config) $ canonicalizePath p
        srcPath <- liftIO $ liftA2 (++) (pure $ outputDir config </> concatDir config) $ canonicalizePath p
        let compared = ccmp (topMatches config) prints past
        liftIO $ removeDir dstPath >> createDirectoryIfMissing True dstPath
        liftIO . createDirectoryIfMissing True $ dstPath </> "compare"
        liftIO . BS.writeFile (dstPath </> "index.html") . renderHtml . renderPage config $ renderTable config compared
        liftIO $ mapM_ (writeCmp config (dstPath </> "compare") srcPath) compared
    where ccmp n ps pst = take n . sortBy (\(x, _, _) (y, _, _) -> compare y x) $ crossCompare ps pst
          writeCmp :: (Show a, Eq a) => Config -> FilePath -> FilePath -> (Double, (Fingerprints, a), (Fingerprints, a)) -> IO ()
          writeCmp c dp sp cmp@(_, (_, t), (_, t')) = renderCompare sp cmp >>= BS.writeFile (dp </> sq t ++ "_" ++ sq t' ++ ".html") . renderHtml . renderPage c
