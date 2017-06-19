module Lichen.Plagiarism.Walk where

import System.Directory

import Data.Hashable
import qualified Data.ByteString as BS

import Lichen.Config
import Lichen.Lexer
import Lichen.Plagiarism.Winnow

-- Given language configuration and the path to a directory containing
-- source files, lex, fingerprint, and winnow each file and associate the
-- resulting fingerprint sets with the path to their file of origin.
fingerprintDir :: Hashable a => Language a -> FilePath -> IO [(Fingerprints, FilePath)]
fingerprintDir lang dir = do
        base <- listDirectory dir
        contents <- mapM BS.readFile $ (\x -> dir ++ x) <$> base
        let pathAssoc = processCode lang <$> base
        purify $ zip (zipWith ($) pathAssoc contents) base
    where purify ((Left e, _):ps) = printLexError e >> purify ps
          purify ((Right e, t):ps) = ((e, t):) <$> purify ps
          purify [] = pure []
