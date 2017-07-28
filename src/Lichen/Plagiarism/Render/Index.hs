{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.Index where

import Data.Foldable
import Data.Monoid ((<>))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Numeric

import Lichen.Util
import Lichen.Config.Plagiarism
import Lichen.Plagiarism.Render

renderEntry :: Show a => Config -> (Double, (b, a), (b, a)) -> H.Html
renderEntry config (match, (_, x), (_, y)) = H.tr (H.td (H.a ! A.href (runPathGenerator (pathGenerator config) config (sq x) (sq y)) $ H.toHtml $ showFFloat (Just 2) (match * 100) "%")
                                                   <> H.td (hs x) <> H.td (hs y))

renderTable :: Show a => Config -> [(Double, (b, a), (b, a))] -> H.Html
renderTable config t = H.table ! A.class_ "table" $ mconcat
    [ H.div ! A.class_ "container" $ mconcat
        [ H.div ! A.class_ "row" $ mconcat
            [ H.tr (H.td "Match" <> H.td "Student A" <> H.td "Student B")
            , traverse_ (renderEntry config) t
            ]
        ]
    ]
