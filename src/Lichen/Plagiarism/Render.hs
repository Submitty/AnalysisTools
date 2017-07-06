{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lichen.Plagiarism.Render where

import Data.Monoid ((<>))

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Size as C.S
import qualified Clay.Render as C.R
import qualified Clay.Text as C.T
import qualified Clay.Font as C.F

import Language.Javascript.JMacro

import Lichen.Config.Plagiarism

hs :: Show a => a -> H.Html
hs = H.toHtml . show

stylesheet :: C.Css
stylesheet = mconcat
    [ ".centered" ? C.textAlign C.center
    , ".highlight" ? C.color C.white <> C.backgroundColor C.grey
    , ".scrollable-pane" ? mconcat
        [ C.width $ C.S.pct 100
        , C.overflowY C.scroll
        , C.position C.absolute
        , C.top $ C.S.px 0
        , C.whiteSpace C.T.pre
        , C.fontFamily [] [C.F.monospace]
        ]
    ]

javascript :: JStat
javascript = [jmacro|
    var x = 0;
    $("#left > .highlight").each(function() {
        $(this).on("click", function(_) {
            var hash = $(this).data("hash");
            var poss = $(".highlight[data=" + hash + "]").positionedOffset();
            $(".left").scrollTop = poss[1];
        });
    });
|]

renderPage :: Config -> H.Html -> H.Html
renderPage config b = H.docTypeHtml $ mconcat
    [ H.head $ mconcat
        [ H.meta ! A.charset "utf-8"
        , H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
        , H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
        , H.title . H.toHtml $ reportTitle config
        , H.link ! A.rel "stylesheet" ! A.href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
        , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
        ]
    , H.body $ mconcat
        [ b
        , H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" $ ""
        , H.script ! A.src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js" $ ""
        , H.script . hs $ renderJs javascript
        ]
    ]
