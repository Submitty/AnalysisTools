{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Lichen.Config.Plagiarism

hs :: Show a => a -> H.Html
hs = H.toHtml . show

renderPage :: Config -> H.Html -> H.Html
renderPage config b = H.docTypeHtml $ mconcat
    [ H.head $ mconcat
        [ H.meta ! A.charset "utf-8"
        , H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
        , H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
        , H.title . H.toHtml $ reportTitle config
        , H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        ]
    , H.body $ mconcat
        [ b
        , H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" $ ""
        , H.script ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" $ ""
        ]
    ]
