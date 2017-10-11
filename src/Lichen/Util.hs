{-# LANGUAGE OverloadedStrings #-}

module Lichen.Util where

import System.Directory
import System.FilePath
import System.Console.ANSI
import System.IO (hPutStr, hPutStrLn, stderr)

import Data.List.Split
import Data.Monoid ((<>))

import Control.Monad
import Control.Monad.IO.Class

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
containingDir [] = []
containingDir p = (if head p == '/' then ('/':) else id) . foldr1 (</>) . init . splitOn "/" $ p

removeDir :: FilePath -> IO ()
removeDir dir = doesDirectoryExist dir >>= flip when (removeDirectoryRecursive dir)

readSafe :: Applicative f => (FilePath -> IO a) -> f a -> FilePath -> IO (f a)
readSafe r e p = doesFileExist p >>= \b -> if b then pure <$> r p else pure e

headSafe :: [a] -> Maybe a
headSafe [] = Nothing
headSafe (x:_) = Just x

(.%) :: (a -> b -> c) -> (c -> d -> e) -> (a -> b -> d -> e)
(.%) f g x y = g (f x y)

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) = (++) .% (++)

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) x y = (+++) (x ++ y)

(+:) :: a -> a -> [a]
(+:) x y = x:[y]

possibly :: Maybe a -> [a]
possibly (Just x) = [x]
possibly Nothing = []

sq :: Show a => a -> String
sq = go . show where
    go ('"':s) | last s == '"' = init s
               | otherwise = s
    go x = x

printColor :: MonadIO m => Color -> m () -> m ()
printColor c body = liftIO (hSetSGR stderr [SetColor Foreground Vivid c]) >> body >> liftIO (hSetSGR stderr [Reset])

notify :: MonadIO m => m () -> m ()
notify = printColor Green

warn :: MonadIO m => m () -> m ()
warn = printColor Yellow

err :: MonadIO m => m () -> m ()
err = printColor Red

progress :: MonadIO m => String -> m a -> m a
progress msg body = do
        liftIO . hPutStr stderr $ msg <> "... "
        ret <- body
        liftIO . notify $ hPutStrLn stderr "Done!"
        return ret
