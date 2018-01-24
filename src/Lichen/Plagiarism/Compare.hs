module Lichen.Plagiarism.Compare where

import Data.List
import qualified Data.Set as Set

import Lichen.Lexer
import Lichen.Plagiarism.Submitty

-- Naively compare two sets of fingerprints to obtain a percent match.
compareFingerprints :: (([Fingerprint], a), ([Fingerprint], a)) -> (Double, ([Fingerprint], a), ([Fingerprint], a))
compareFingerprints ((al, x), (bl, y)) = (fromIntegral (Set.size is) / fromIntegral (Set.size un), (matching is al, x), (matching is bl, y)) where
    matching :: Set.Set Int -> [Fingerprint] -> [Fingerprint]
    matching s = filter (flip Set.member s . tdata)
    a = Set.fromList (tdata <$> al)
    b = Set.fromList (tdata <$> bl)
    is = Set.intersection a b
    un = Set.union a b

-- Given a list of pairs of fingerprints and an associated tag (typically
-- the source file from which those fingeprints were generated), along with
-- another list of tagged past fingerprints, compare each possible pair of
-- fingerprints, returning a list of percent matches associated with the tags
-- of the two fingeprint sets compared.
crossCompare :: [([Fingerprint], a)] -> [([Fingerprint], a)] -> [(Double, ([Fingerprint], a), ([Fingerprint], a))]
crossCompare prints past = compareFingerprints <$> (pairs prints ++ oldpairs)
    where pairs :: [([Fingerprint], a)] -> [(([Fingerprint], a), ([Fingerprint], a))]
          pairs lst = tails lst >>= subpairs
          subpairs [] = []
          subpairs (x:xs) = (\y -> (x, y)) <$> xs
          oldpairs = [(x, y) | x <- prints, y <- past]

