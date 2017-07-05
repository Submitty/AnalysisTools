{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.Compare where

import System.FilePath

import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

import Text.Megaparsec.Pos
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Lichen.Util
import Lichen.Lexer
import Lichen.Plagiarism.Winnow

data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Show, Eq)
data Colored = Uncolored T.Text | Colored Color T.Text deriving (Show, Eq)

colorize :: Colored -> H.Html
colorize (Uncolored t) = H.toHtml t
colorize (Colored Red t) = H.span ! A.class_ "highlight red" $ H.toHtml t
colorize (Colored Orange t) = H.span ! A.class_ "highlight orange" $ H.toHtml t
colorize (Colored Yellow t) = H.span ! A.class_ "highlight yellow" $ H.toHtml t
colorize (Colored Green t) = H.span ! A.class_ "highlight green" $ H.toHtml t
colorize (Colored Blue t) = H.span ! A.class_ "highlight blue" $ H.toHtml t
colorize (Colored Indigo t) = H.span ! A.class_ "highlight indigo" $ H.toHtml t
colorize (Colored Violet t) = H.span ! A.class_ "highlight violet" $ H.toHtml t

deoverlap :: [(Int, Int)] -> [(Int, Int)]
deoverlap [] = []
deoverlap [x] = [x]
deoverlap ((s, f):(s', f'):xs) | f > s' && f < f' = (s, s'):deoverlap ((s', f'):xs)
                               | f > f' = (s, s'):deoverlap ((s', f'):(f, f'):xs)
                               | otherwise = (s, f):deoverlap ((s', f'):xs)

splitInto :: T.Text -> [(Int, Int)] -> [Colored]
splitInto = go 0 where
    go _ s [] | T.null s = []
              | otherwise = [Uncolored s]
    go off s ((sp, ep):ps) = if T.null preTok
                                 then Colored Red tok:go ep postTok ps
                                 else Uncolored preTok:Colored Red tok:go ep postTok ps
        where (preTok, preTokRest) = T.splitAt (sp - off) s
              (tok, postTok) = T.splitAt (ep - sp) preTokRest

expandTabs :: T.Text -> T.Text
expandTabs = T.replace "\t" "        "

-- TODO - many of these functions are called much more than necessary. This
-- is currently a debugging consideration. Many of these should be moved
-- into a where clause in renderTagged so as to eliminate the "s" argument.
lineColToAbs :: T.Text -> Int -> Int -> Int
lineColToAbs s l c = c + (l - 2) + sum (T.length <$> take (l - 1) ls) where
    ls = T.lines s

convertPos :: T.Text -> TokPos -> (Int, Int)
convertPos s tp = (spos, epos) where
    spos = lineColToAbs s (fromIntegral . unPos $ startLine tp) (fromIntegral . unPos $ startCol tp)
    epos = lineColToAbs s (fromIntegral . unPos $ endLine tp) (fromIntegral . unPos $ endCol tp)

renderSource :: T.Text -> [TokPos] -> H.Html
renderSource s p = mconcat . fmap colorize . splitInto es . deoverlap . sort $ fmap (convertPos es) p where es = expandTabs s

renderTagged :: Show a => FilePath -> (Fingerprints, a) -> IO H.Html
renderTagged dir (fp, t) = flip renderSource (tpos <$> fp) <$> T.IO.readFile (dir </> sq t) 

renderCompare :: Show a => FilePath -> (Double, (Fingerprints, a), (Fingerprints, a)) -> IO H.Html
renderCompare dir (m, g@(_, t), g'@(_, t')) = do
        s <- renderTagged dir g
        s' <- renderTagged dir g'
        return $ H.div ! A.class_ "container" $ mconcat
            [ H.h1 ! A.class_ "centered" $ H.toHtml (sq t ++ " vs. " ++ sq t' ++ ": " ++ show (m * 100) ++ "% match")
            , H.div ! A.class_ "row" $ mconcat
                [ H.div ! A.class_ "col-sm-6"
                    $ H.div ! A.id "left" ! A.class_ "scrollable-pane" $ s
                , H.div ! A.class_ "col-sm-6"
                    $ H.div ! A.id "right" ! A.class_ "scrollable-pane" $ s'
                ]
            ]
