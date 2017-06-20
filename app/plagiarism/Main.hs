{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Loader

main :: IO ()
main = runPlagiarism configC
