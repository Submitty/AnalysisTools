{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.Index where

import Data.Foldable
import Data.Monoid ((<>))

import qualified Text.Blaze.Html5 as H

import Lichen.Plagiarism.Render

renderEntry :: Show a => (Double, (b, a), (b, a)) -> H.Html
renderEntry (match, (_, x), (_, y)) = H.tr (H.td (hs match) <> H.td (hs x) <> H.td (hs y))

renderTable :: Show a => [(Double, (b, a), (b, a))] -> H.Html
renderTable t = header <> traverse_ renderEntry t where
    header = H.tr (H.td "Match" <> H.td "Student A" <> H.td "Student B")
