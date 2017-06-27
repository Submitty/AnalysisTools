module Lichen.Plagiarism.Walk where

import System.Directory
import System.FilePath

import qualified Data.ByteString as BS

import Control.Monad.Reader

import Lichen.Config.Languages
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Winnow

-- Given language configuration and the path to a directory containing
-- source files, lex, fingerprint, and winnow each file and associate the
-- resulting fingerprint sets with the path to their file of origin.
fingerprintDir :: Language -> FilePath -> Plagiarism [(Fingerprints, FilePath)]
fingerprintDir lang dir = do
        base <- liftIO $ listDirectory dir
        contents <- liftIO . mapM BS.readFile $ (\x -> dir </> x) <$> base
        let pathAssoc = processCode lang <$> base
        mapM clean $ zip (zipWith ($) pathAssoc contents) base
    where clean (p, f) = (,) <$> p <*> pure f
