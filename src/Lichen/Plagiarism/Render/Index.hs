{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.Index where

import Data.Foldable
import Data.Monoid ((<>))

import qualified Text.Blaze.Html5 as H

import Lichen.Plagiarism.Render

entry :: Show a => (Double, a, a) -> H.Html
entry (match, x, y) = H.tr (H.td (hs match) <> H.td (hs x) <> H.td (hs y))

table :: Show a => [(Double, a, a)] -> H.Html
table t = header <> traverse_ entry t where
    header = H.tr (H.td "Match" <> H.td "Student A" <> H.td "Student B")
