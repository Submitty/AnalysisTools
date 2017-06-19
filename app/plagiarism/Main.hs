{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment
import System.Directory

import Data.List
import Data.Monoid (mconcat, (<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Control.Monad

import Lichen.Config
import Lichen.Plagiarism.Winnow
import Lichen.Plagiarism.Compare
import Lichen.Plagiarism.Walk
import Lichen.Plagiarism.Concatenate
import Lichen.Plagiarism.Highlight
import Lichen.Lexer
import qualified Lichen.Lexer.C as C

langC = Language ["c", "h", "cpp", "hpp", "C", "H", "cc"]
                 C.lex $ WinnowConfig 9 5

main :: IO ()
main = do
        [p] <- getArgs
        dir <- canonicalizePath p
        concatenate dir
        highlight dir
        prints <- fingerprintDir langC ("plagiarism_data/concatenated" ++ dir)
        print $ crossCompare langC prints
