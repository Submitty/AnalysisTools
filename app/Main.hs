{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Data.List
import Data.Monoid (mconcat, (<>))
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T.IO

import Control.Monad

import Lexer
import Config
import Language
import Winnow
import Compare
import Walk

langC = Language "c" ["c", "h", "cpp", "hpp", "C", "H", "cc"]
                 $ Config (256*256) 9 5

basename = last . T.splitOn "/"
stripext = head . T.splitOn "."

tablify :: [(Double, T.Text, T.Text)] -> T.Text
tablify l = "<table border=\"1\">" <> mconcat (map rowify l) <> "</table>" where
    rowify (d, a, b) = mconcat ["<tr><td><a href=", stripext $ basename a, "_", stripext $ basename b, ".html>", T.pack $ show d, "</a></td><td>", basename a, "</td><td>", basename b, "</td></tr>"]

comparify :: T.Text -> T.Text -> T.Text
comparify a b = mconcat [ "<html><head><link rel=\"stylesheet\" href=\"static/style.css\"></head><body><div class=\"left-div\"><iframe src=\"highlighted/"
                        , a
                        , ".html\" width=\"100%\" height=\"100%\"></iframe></div><div class=\"right-div\"><iframe src=\"highlighted/"
                        , b
                        , ".html\" width=\"49%\" height=\"100%\"></iframe></div></body></html>"
                        ]

generateCompare :: T.Text -> T.Text -> IO ()
generateCompare a b = T.IO.writeFile (T.unpack ("plagiarism_data/" <> stripext (basename a) <> "_" <> stripext (basename b) <> ".html")) $ comparify (basename a) (basename b)

main :: IO ()
main = do
        (root:_) <- getArgs
        sorted <- reverse . sortOn (\(t, _, _) -> t) . crossCompare langC <$> readyWithinSourceDir langC root
        let short = take 100 sorted in do
            T.IO.writeFile "plagiarism_data/index.html" $ tablify short
            mapM_ (\(_, a, b) -> generateCompare a b) short

