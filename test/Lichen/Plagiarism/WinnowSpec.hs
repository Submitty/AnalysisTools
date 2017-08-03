module Lichen.Plagiarism.WinnowSpec where

import Lichen.Plagiarism.Winnow

import qualified Data.List.NonEmpty as NE

import Test.Hspec
import Test.QuickCheck

prop_windows_size :: Int -> [Int] -> Bool
prop_windows_size k l | k > 0 && k <= length l = case windows k l of
                                                     Just ws -> all (\x -> NE.length x == k) ws
                                                     Nothing -> False
                      | otherwise = case windows k l of Just _ -> False; Nothing -> True

spec :: Spec
spec = describe "windows" $ it "produces windows of length k for positive k less than length of l" $ property prop_windows_size
