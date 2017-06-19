module Lichen.Util where

import Data.List.Split

import Lichen.Config

-- Ex: purify [Maybe 1, Nothing, Maybe 3] = [1, 3]
purify :: [Maybe a] -> [a]
purify [] = []
purify (Nothing:xs) = purify xs
purify (Just x:xs) = x:purify xs

-- Ex: purifyFst [(Maybe 1, 2), (Nothing, 4), (Maybe 5, 6)] = [(1,2),(5,6)]
purifyFst :: [(Maybe a, b)] -> [(a, b)]
purifyFst [] = []
purifyFst ((Nothing, _):xs) = purifyFst xs
purifyFst ((Just x, y):xs) = (x, y):purifyFst xs

-- Ex: purifySnd [(1, Maybe 2), (3, Nothing), (5, Maybe 6)] = [(1,2),(5,6)]
purifySnd :: [(a, Maybe b)] -> [(a, b)]
purifySnd [] = []
purifySnd ((_, Nothing):xs) = purifySnd xs
purifySnd ((x, Just y):xs) = (x, y):purifySnd xs

-- Ex: containingDir "/usr/bin/gcc" = "/usr/bin"
containingDir :: FilePath -> FilePath
containingDir = foldr1 (\x y -> x ++ "/" ++ y) . init . splitOn "/"
