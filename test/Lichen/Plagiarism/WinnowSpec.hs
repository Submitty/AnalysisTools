module Lichen.Plagiarism.WinnowSpec where

import Lichen.Plagiarism.Winnow

import qualified Data.List.NonEmpty as NE

import Text.Megaparsec

import Test.Hspec
import Test.QuickCheck

import Lichen.Lexer

prop_windows_size :: Int -> [Int] -> Bool
prop_windows_size k l | k > 0 && k <= length l = case windows k l of
                                                     Just ws -> all (\x -> NE.length x == k) ws
                                                     Nothing -> False
                      | otherwise = case windows k l of Just _ -> False; Nothing -> True

prop_windows_count :: Int -> [Int] -> Bool
prop_windows_count k l | k > 0 && k <= length l = case windows k l of
                                                      Just ws -> length ws == length l - k + 1
                                                      Nothing -> False
                       | otherwise = case windows k l of Just _ -> False; Nothing -> True

instance Arbitrary TokPos where
        arbitrary = TokPos <$> (unsafePos <$> (getPositive <$> (arbitrary :: Gen (Positive Word))))
                           <*> (unsafePos <$> (getPositive <$> (arbitrary :: Gen (Positive Word))))
                           <*> (unsafePos <$> (getPositive <$> (arbitrary :: Gen (Positive Word))))
                           <*> (unsafePos <$> (getPositive <$> (arbitrary :: Gen (Positive Word))))

instance Arbitrary a => Arbitrary (Tagged a) where
        arbitrary = Tagged <$> arbitrary <*> arbitrary

prop_hashWin_bounds :: NE.NonEmpty (Tagged Int) -> Bool
prop_hashWin_bounds l = let r = hashWin l in startLine (tpos r) == startLine (tpos $ NE.head l)
                                          && startCol (tpos r) == startCol (tpos $ NE.head l)
                                          && endLine (tpos r) == endLine (tpos $ NE.last l)
                                          && endCol (tpos r) == endCol (tpos $ NE.last l)

spec :: Spec
spec = do
        describe "windows k l" $ do
            it "produces windows of length k for positive k less than length of l" $ property prop_windows_size
            it "produces length l - k + 1 windows" $ property prop_windows_count
        describe "hashWin l" $ do
            it "yields a fingerprint with the same bounds as the window l" $ property prop_hashWin_bounds
