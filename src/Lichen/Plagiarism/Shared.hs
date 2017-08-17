module Lichen.Plagiarism.Shared where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Winnow

findShared :: Config -> [Fingerprints] -> [Fingerprints] -> Set.Set Fingerprint
findShared c prints past = Map.keysSet $ Map.filter (\x -> fromIntegral x / fromIntegral st >= sharedThreshold c) tallied where
    tally :: Fingerprints -> Map.Map Fingerprint Int -> Map.Map Fingerprint Int
    tally ps m = foldr (Map.update (Just . succ)) m ps
    tallied = foldr tally Map.empty $ prints ++ past
    st = Map.size tallied
