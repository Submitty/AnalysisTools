{-# LANGUAGE OverloadedStrings #-}

module Walk where

import System.Directory

import Data.List
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T.IO
import qualified Data.Text.Lazy.Encoding as T.E
import qualified Data.Text.Encoding.Error as T.E.E

import Language
import Winnow
import Compare

findSourceDirs :: Language -> FilePath -> IO [FilePath]
findSourceDirs l root = do
        fs <- map (\x -> root ++ "/" ++ x) <$> listDirectory root
        if isSourceDir l $ map T.pack fs
            then return [root]
            else do
                areDir <- mapM doesDirectoryExist fs
                let dirs = map fst $ filter snd $ zip fs areDir in do
                    children <- mapM (findSourceDirs l) dirs
                    return $ concat children

condenseSourceDir :: Language -> FilePath -> IO T.Text
condenseSourceDir l root = do
        fs <- filter (isSource l . T.pack) . map (\x -> root ++ "/" ++ x) <$> listDirectory root
        bs <- mapM BS.readFile fs
        return $ foldr1 mappend $ map (T.E.decodeUtf8With T.E.E.lenientDecode) bs

nameSourceDir :: FilePath -> T.Text
nameSourceDir = (\[a, b] -> a `mappend` "_" `mappend` b) . reverse . take 2 . reverse . T.splitOn "/" . T.pack

readySources :: Language -> FilePath -> IO [(T.Text, Fingerprints)]
readySources l root = do
        dirs <- findSourceDirs l root
        srcs <- mapM (condenseSourceDir l) dirs
        prints <- mapM (processCode l) srcs
        return $ zip (nameSourceDir <$> dirs) prints

readyWithinSourceDir :: Language -> FilePath -> IO [(T.Text, Fingerprints)]
readyWithinSourceDir l dir = do
        fs <- map (\x -> dir ++ "/" ++ x) <$> listDirectory dir
        srcs <- map (T.E.decodeUtf8With T.E.E.lenientDecode) <$> mapM BS.readFile fs
        prints <- mapM (processCode l) srcs
        return $ zip (T.pack <$> fs) prints

crossCompare :: Language -> [(T.Text, Fingerprints)] -> [(Double, T.Text, T.Text)]
crossCompare l prints = map cmp $ pairs prints
    where cmp ((n1, f1), (n2, f2)) = (compareFingerprints f1 f2, n1, n2)
          pairs lst = tails lst >>= subpairs
          subpairs [] = []
          subpairs (x:xs) = map (\y -> (x, y)) xs
