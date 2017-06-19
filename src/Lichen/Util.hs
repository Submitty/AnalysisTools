module Lichen.Util where

import Data.List.Split

purify :: [Maybe a] -> [a]
purify [] = []
purify (Nothing:xs) = purify xs
purify (Just x:xs) = x:purify xs

purifyFst :: [(Maybe a, b)] -> [(a, b)]
purifyFst [] = []
purifyFst ((Nothing, _):xs) = purifyFst xs
purifyFst ((Just x, y):xs) = (x, y):purifyFst xs

purifySnd :: [(a, Maybe b)] -> [(a, b)]
purifySnd [] = []
purifySnd ((_, Nothing):xs) = purifySnd xs
purifySnd ((x, Just y):xs) = (x, y):purifySnd xs

containingDir :: FilePath -> FilePath
containingDir = foldr1 (\x y -> x ++ "/" ++ y) . init . splitOn "/"
