{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.Compare where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Lichen.Util
import Lichen.Plagiarism.Winnow

renderCompare :: Show a => (Double, (Fingerprints, a), (Fingerprints, a)) -> H.Html
renderCompare (m, (_, t), (_, t')) = H.div ! A.class_ "container" $ mconcat
    [ H.h1 ! A.class_ "centered" $ H.toHtml (sq t ++ " vs. " ++ sq t' ++ ": " ++ sq (m * 100) ++ "% Match")
    , H.div ! A.class_ "row" $ mconcat
        [ H.div ! A.class_ "col-sm-6"
            $ H.div ! A.id "left" ! A.class_ "scrollable-pane" $ mconcat
                [ H.div ! A.id "112324124345543757457586456" ! A.class_ "highlight green" $ "hello"
                ]
        , H.div ! A.class_ "col-sm-6"
            $ H.div ! A.id "right" ! A.class_ "scrollable-pane" $ mconcat
                [ "hello"
                ]
        ]
    ]
