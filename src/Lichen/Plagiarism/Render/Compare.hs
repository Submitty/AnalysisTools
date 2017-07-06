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

data Colored a = Uncolored T.Text | Colored a T.Text deriving (Show, Eq)

colorize :: Show a => Colored a -> H.Html
colorize (Uncolored t) = H.toHtml t
colorize (Colored x t) = H.span ! A.class_ "highlight" ! H.dataAttribute "hash" (H.stringValue $ show x) $ H.toHtml t

deoverlap :: [((Int, Int), a)] -> [((Int, Int), a)]
deoverlap [] = []
deoverlap [x] = [x]
deoverlap (((s, f), x):((s', f'), x'):xs) | f > s' && f < f' = ((s, s'), x):deoverlap (((s', f'), x'):xs)
                               | f > f' = ((s, s'), x):deoverlap (((s', f'), x'):((f', f), x):xs)
                               | otherwise = ((s, f), x):deoverlap (((s', f'), x'):xs)

splitInto :: T.Text -> [((Int, Int), a)] -> [Colored a]
splitInto = go 0 where
    go _ s [] | T.null s = []
              | otherwise = [Uncolored s]
    go off s (((sp, ep), x):ps) = if T.null preTok
                                 then Colored x tok:go ep postTok ps
                                 else Uncolored preTok:Colored x tok:go ep postTok ps
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

convertPos :: T.Text -> Tagged a -> ((Int, Int), a)
convertPos s (Tagged x tp) = ((spos, epos), x) where
    spos = lineColToAbs s (fromIntegral . unPos $ startLine tp) (fromIntegral . unPos $ startCol tp)
    epos = lineColToAbs s (fromIntegral . unPos $ endLine tp) (fromIntegral . unPos $ endCol tp)

renderSource :: Show a => T.Text -> [Tagged a] -> H.Html
renderSource s p = mconcat . fmap colorize . splitInto es . deoverlap . sortBy (\a b -> compare (fst a) (fst b)) $ fmap (convertPos es) p where es = expandTabs s

renderTagged :: Show a => FilePath -> (Fingerprints, a) -> IO H.Html
renderTagged dir (fp, t) = flip renderSource fp <$> T.IO.readFile (dir </> sq t) 

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
