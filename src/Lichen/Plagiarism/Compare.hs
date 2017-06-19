module Lichen.Plagiarism.Compare where

import Data.List
import qualified Data.Set as Set

import Lichen.Config
import Lichen.Plagiarism.Winnow

-- Naively compare two sets of fingerprints to obtain a percent match.
compareFingerprints :: Fingerprints -> Fingerprints -> Double
compareFingerprints a b = fromIntegral (Set.size (Set.intersection a b)) / fromIntegral (Set.size (Set.union a b))

-- Given a list of pairs of fingerprints and an associated tag (typically
-- the source file from which those fingeprints were generated), compare
-- each possible pair of fingerprints, returning a list of percent matches
-- associated with the tags of the two fingeprint sets compared.
crossCompare :: [(Fingerprints, a)] -> [(Double, a, a)]
crossCompare prints = map cmp $ pairs prints
    where cmp ((f1, n1), (f2, n2)) = (compareFingerprints f1 f2, n1, n2)
          pairs lst = tails lst >>= subpairs
          subpairs [] = []
          subpairs (x:xs) = map (\y -> (x, y)) xs
