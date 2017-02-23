module Winnow where

import qualified Data.Set as Set
import qualified Data.Text.Lazy as T

import Config
import Lexer
import Language

type Fingerprint = Int
type Fingerprints = Set.Set Fingerprint

hash :: Int -> [Token] -> Fingerprint
hash bound = flip mod bound . go where
    go [] = 5381
    go ((x, _):xs) = x + 33 * go xs

windows :: Int -> [a] -> [[a]]
windows _ [] = []
windows size lst@(x:xs) | length lst >= size = take size lst:windows size xs
                        | otherwise = []

fingerprint :: ([Token] -> Fingerprint) -> Int -> [Token] -> [Fingerprint]
fingerprint h k lst = h <$> windows k lst

winnow :: Int -> Int -> [Fingerprint] -> Fingerprints
winnow t k lst = Set.fromList $ go [] allWindows where
    allWindows :: [[(Int, Int)]]
    allWindows = windows (t - k + 1) $ zip lst [1, 2 ..]
    go :: [(Int, Int)] -> [[(Int, Int)]] -> [Int]
    go acc [] = fst $ unzip acc
    go acc (w:ws) =
        let uz = unzip acc
            fil = filter (\(_, n) -> notElem n $ snd uz) w in
                if null fil
                    then go acc ws
                    else let mf = minimum fil
                             mo = minimum w in
                                 if mf == mo then go (minimum fil:acc) ws
                                             else go acc ws

processTokens :: Config -> [Token] -> Fingerprints
processTokens config = winnow (signalThreshold config) (noiseThreshold config)
                     . fingerprint (hash $ hashBound config) (noiseThreshold config)

processCode :: Language -> T.Text -> IO Fingerprints
processCode l@(Language _ _ c) src = processTokens c <$> runLexer l src
