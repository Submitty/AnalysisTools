{-# LANGUAGE OverloadedStrings #-}

module Lichen.Config where

import qualified Data.Text as T

import Lichen.Lexer

data WinnowConfig = WinnowConfig
            { signalThreshold :: Int
            , noiseThreshold :: Int
            }

data Language a = Language T.Text [T.Text] (Lexer a) WinnowConfig

data Config = Config
            {
            }

isSource :: Language a -> T.Text -> Bool
isSource (Language _ exts _ _) path = last (T.splitOn "." path) `elem` exts

isSourceDir :: Language a -> [T.Text] -> Bool
isSourceDir l = any (isSource l)
