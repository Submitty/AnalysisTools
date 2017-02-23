{-# LANGUAGE OverloadedStrings #-}

module Language where

import qualified Data.Text.Lazy as T

import Config

data Language = Language T.Text [T.Text] Config

isSource :: Language -> T.Text -> Bool
isSource (Language _ exts _) path = last (T.splitOn "." path) `elem` exts

isSourceDir :: Language -> [T.Text] -> Bool
isSourceDir l = any (isSource l)
