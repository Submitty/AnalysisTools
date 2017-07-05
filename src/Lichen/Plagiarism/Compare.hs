module Lichen.Plagiarism.Compare where

import Data.List
import qualified Data.Set as Set

import Lichen.Plagiarism.Winnow

-- Naively compare two sets of fingerprints to obtain a percent match.
compareFingerprints :: ((Fingerprints, a), (Fingerprints, a)) -> (Double, (Fingerprints, a), (Fingerprints, a))
compareFingerprints ((al, x), (bl, y)) = (fromIntegral (Set.size un) / fromIntegral (Set.size is), (Set.toList is, x), (Set.toList is, y)) where
    a = Set.fromList al
    b = Set.fromList bl
    is = Set.intersection a b
    un = Set.union a b

-- Given a list of pairs of fingerprints and an associated tag (typically
-- the source file from which those fingeprints were generated), compare
-- each possible pair of fingerprints, returning a list of percent matches
-- associated with the tags of the two fingeprint sets compared.
crossCompare :: [(Fingerprints, a)] -> [(Double, (Fingerprints, a), (Fingerprints, a))]
crossCompare prints = compareFingerprints <$> pairs prints
    where pairs :: [(Fingerprints, a)] -> [((Fingerprints, a), (Fingerprints, a))]
          pairs lst = tails lst >>= subpairs
          subpairs [] = []
          subpairs (x:xs) = (\y -> (x, y)) <$> xs
