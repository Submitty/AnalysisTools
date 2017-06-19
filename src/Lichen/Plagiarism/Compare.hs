module Lichen.Plagiarism.Compare where

import Data.List
import qualified Data.Set as Set

import Lichen.Config
import Lichen.Plagiarism.Winnow

compareFingerprints :: Fingerprints -> Fingerprints -> Double
compareFingerprints a b = fromIntegral (Set.size (Set.intersection a b)) / fromIntegral (Set.size (Set.union a b))

crossCompare :: Language a -> [(Fingerprints, FilePath)] -> [(Double, FilePath, FilePath)]
crossCompare l prints = map cmp $ pairs prints
    where cmp ((f1, n1), (f2, n2)) = (compareFingerprints f1 f2, n1, n2)
          pairs lst = tails lst >>= subpairs
          subpairs [] = []
          subpairs (x:xs) = map (\y -> (x, y)) xs
