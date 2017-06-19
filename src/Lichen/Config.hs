{-# LANGUAGE OverloadedStrings #-}

module Lichen.Config where

import Data.List.Split

import Lichen.Lexer

data WinnowConfig = WinnowConfig
            { signalThreshold :: Int
            , noiseThreshold :: Int
            }

data Language a = Language [FilePath] (Lexer a) WinnowConfig

data Config = Config
            {
            }

isSource :: Language a -> FilePath -> Bool
isSource (Language exts _ _) path = last (splitOn "." path) `elem` exts

isSourceDir :: Language a -> [FilePath] -> Bool
isSourceDir l = any (isSource l)
