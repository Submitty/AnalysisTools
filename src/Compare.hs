module Compare where

import qualified Data.Set as Set

import Config
import Winnow

compareFingerprints :: Fingerprints -> Fingerprints -> Double
compareFingerprints a b = fromIntegral (Set.size (Set.intersection a b)) / fromIntegral (Set.size (Set.union a b))
