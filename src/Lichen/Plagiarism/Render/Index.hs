{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.Index where

import Data.Foldable
import Data.Monoid ((<>))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Numeric

import Lichen.Plagiarism.Submitty
import Lichen.Plagiarism.Render

renderEntry :: Show a => Semester -> Course -> Assignment -> (Double, (b, a), (b, a)) -> H.Html
renderEntry _ _ _ (match, (_, x), (_, y)) = H.tr (H.td (H.a ! A.href "TODO" $ H.toHtml $ showFFloat (Just 2) (match * 100) "%")
                                                  <> H.td (hs x) <> H.td (hs y))

renderTable :: Show a => Semester -> Course -> Assignment -> [(Double, (b, a), (b, a))] -> H.Html
renderTable semester course assignment table = H.table ! A.class_ "table" $ mconcat
    [ H.div ! A.class_ "container" $ mconcat
        [ H.div ! A.class_ "row" $ mconcat
            [ H.tr (H.td "Match" <> H.td "Student A" <> H.td "Student B")
            , traverse_ (renderEntry semester course assignment) table
            ]
        ]
    ]
