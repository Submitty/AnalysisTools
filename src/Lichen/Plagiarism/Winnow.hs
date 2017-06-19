module Lichen.Plagiarism.Winnow where

import Data.Hashable
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Lichen.Config
import Lichen.Lexer

type Fingerprint = Int
type Fingerprints = Set.Set Fingerprint

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows size lst@(x:xs) | length lst >= size = take size lst:windows size xs
                        | otherwise = []

fingerprint :: Hashable a => Int -> [a] -> [Fingerprint]
fingerprint k lst = hash <$> windows k lst

winnow :: Int -> Int -> [Fingerprint] -> Fingerprints
winnow t k lst = Set.fromList $ go [] allWindows where
    allWindows :: [[(Int, Int)]]
    allWindows = windows (t - k + 1) $ zip lst [1, 2 ..]
    go :: [(Int, Int)] -> [[(Int, Int)]] -> [Int]
    go acc [] = fst <$> acc
    go acc (w:ws) =
        let uz = unzip acc
            fil = filter (\(_, n) -> notElem n $ snd uz) w in
                if null fil
                    then go acc ws
                    else let mf = minimum fil
                             mo = minimum w in
                                 if mf == mo then go (minimum fil:acc) ws
                                             else go acc ws

processTokens :: Hashable a => WinnowConfig -> [a] -> Fingerprints
processTokens config = winnow (signalThreshold config) (noiseThreshold config)
                     . fingerprint (noiseThreshold config)

processCode :: Hashable a => Language a -> FilePath -> BS.ByteString -> Either LexError Fingerprints
processCode (Language _ lex c) p src = processTokens c <$> lex p src
