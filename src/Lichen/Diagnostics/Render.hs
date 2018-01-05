{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Lichen.Diagnostics.Render where

import Data.List
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Text.Megaparsec.Pos (unPos)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Clay ((?))
import qualified Clay as C
import qualified Clay.Render as C.R
import qualified Clay.Text as C.T
import qualified Clay.Font as C.F

import Language.Javascript.JMacro

import Lichen.Util
import Lichen.Lexer

data Colored a = Uncolored T.Text | Colored a T.Text deriving (Show, Eq)

hs :: Show a => a -> H.Html
hs = H.toHtml . sq

stylesheet :: C.Css
stylesheet = mconcat [ ".centered" ? C.textAlign C.center
                     , ".matches" ? C.color C.white <> C.backgroundColor C.grey
                     , ".hovering" ? C.color C.white <> C.backgroundColor C.blue
                     , ".pane" ? mconcat [ C.whiteSpace C.T.pre
                                         , C.fontFamily [] [C.F.monospace]
                                         ]
                     ]

javascript :: JStat
javascript = [jmacro|
    $(".matches").each(function () {
        $(this).hover(function () { $(this).toggleClass("hovering"); });
    });
|]

colorize :: Show a => Colored a -> H.Html
colorize (Uncolored t) = H.toHtml t
colorize (Colored x t) = H.span ! A.class_ "matches" ! A.title (H.stringValue $ show x) $ H.toHtml t

splitInto :: T.Text -> [((Int, Int), a)] -> [Colored a]
splitInto = go 0 where
    go _ s [] | T.null s = []
              | otherwise = [Uncolored s]
    go off s (((sp, ep), x):ps) = if T.null preTok then Colored x tok:go ep postTok ps else Uncolored preTok:Colored x tok:go ep postTok ps
        where (preTok, preTokRest) = T.splitAt (sp - off) s
              (tok, postTok) = T.splitAt (ep - sp) preTokRest

toPosList :: Show a => T.Text -> [Tagged a] -> [((Int, Int), a)]
toPosList s p = sortBy (\a b -> compare (fst a) (fst b)) $ fmap convertPos p where
    ls = T.lines s
    convertPos :: Tagged a -> ((Int, Int), a)
    convertPos (Tagged x tp) = ((spos, epos), x) where
        spos = lineColToAbs (fromIntegral . unPos $ startLine tp) (fromIntegral . unPos $ startCol tp)
        epos = lineColToAbs (fromIntegral . unPos $ endLine tp) (fromIntegral . unPos $ endCol tp)
    lineColToAbs :: Int -> Int -> Int
    lineColToAbs l c = c + (l - 2) + sum (T.length <$> take (l - 1) ls)

renderTokens :: Show a => FilePath -> [Tagged a] -> IO H.Html
renderTokens path ts = do
        s <- T.IO.readFile path
        let es = T.replace "\t" "        " s
        return . mconcat . fmap colorize . splitInto es $ toPosList es ts

renderPage :: H.Html -> H.Html
renderPage b =
    H.docTypeHtml $ mconcat
        [ H.head $ mconcat
            [ H.meta ! A.charset "utf-8"
            , H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            , H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
            , H.title "Interactive Diagnostics"
            , H.style . H.toHtml $ C.renderWith C.R.compact [] stylesheet
            ]
        , H.body $ mconcat
            [ H.h1 "Token Diagnostics"
            , H.div ! A.class_ "pane" $ b
            , H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js" $ ""
            , H.script . hs $ renderJs javascript
            ]
        ]
