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

import Lichen.Util
import Lichen.Config.Plagiarism

hs :: Show a => a -> H.Html
hs = H.toHtml . sq

stylesheet :: C.Css
stylesheet = mconcat
    [ ".centered" ? C.textAlign C.center
    , ".highlight" ? C.color C.white <> C.backgroundColor C.grey
    , ".hovering" ? C.color C.white <> C.backgroundColor C.blue
    , ".selected-red" ? C.color C.white <> C.backgroundColor C.red
    , ".selected-orange" ? C.color C.white <> C.backgroundColor C.orange
    , ".selected-yellow" ? C.color C.white <> C.backgroundColor C.yellow
    , ".selected-green" ? C.color C.white <> C.backgroundColor C.green
    , ".selected-blue" ? C.color C.white <> C.backgroundColor C.blue
    , ".selected-indigo" ? C.color C.white <> C.backgroundColor C.indigo
    , ".selected-violet" ? C.color C.white <> C.backgroundColor C.violet
    , ".scrollable-pane" ? mconcat
        [ C.width $ C.S.pct 100
        , C.height $ C.S.vh 80
        , C.overflowY C.scroll
        , C.position C.absolute
        , C.top $ C.S.px 0
        , C.whiteSpace C.T.pre
        , C.fontFamily [] [C.F.monospace]
        ]
    ]

javascript :: JStat
javascript = [jmacro|
    var currentHighlight = "selected-red";
    fun nextHighlight cur {
        switch (cur) {
            case "selected-red": return "selected-orange";
            case "selected-orange": return "selected-yellow";
            case "selected-yellow": return "selected-green";
            case "selected-green": return "selected-blue";
            case "selected-blue": return "selected-indigo";
            case "selected-indigo": return "selected-violet";
            case "selected-violet": return "selected-red";
        }
    }
    $("#left > .highlight").each(function() {
        $(this).on("click", function(_) {
            var hash = $(this).data("hash");
            var pos = $("#right > .highlight[data-hash=" + hash + "]")[0].offsetTop;
            $("#right").scrollTop(pos);
        });
    });
    $("#right > .highlight").each(function() {
        $(this).on("click", function(_) {
            var hash = $(this).data("hash");
            var pos = $("#left > .highlight[data-hash=" + hash + "]")[0].offsetTop;
            $("#left").scrollTop(pos);
        });
    });
    $(".highlight").each(function() {
        $(this).hover(function () {
            $(this).toggleClass("hovering");
            var hash = $(this).data("hash");
            var side = $(this).parent("#left").length ? "#right" : "#left";
            $(side + " > .highlight[data-hash=" + hash + "]").toggleClass("hovering");
        });
        $(this).on("contextmenu", function(_) {
            var hash = $(this).data("hash");
            if ($(this).is("*[class*='selected']")) {
                $(".highlight[data-hash=" + hash + "]").removeClass(\_ c -> (c.match(/(^|\s)selected-\S+/g) || []).join(' '));
            } else {
                $(".highlight[data-hash=" + hash + "]").addClass(currentHighlight);
                currentHighlight = nextHighlight(currentHighlight);
            }
            return false;
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
