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

import Numeric

import Lichen.Util
import Lichen.Lexer
import Lichen.Plagiarism.Winnow

data Colored a = Uncolored T.Text | Colored a T.Text deriving (Show, Eq)

colorize :: Show a => Colored a -> H.Html
colorize (Uncolored t) = H.toHtml t
colorize (Colored x t) = H.span ! A.class_ "matches" ! H.dataAttribute "hash" (H.stringValue $ show x) $ H.toHtml t

deoverlap :: [((Int, Int), a)] -> [((Int, Int), a)]
deoverlap [] = []
deoverlap [x] = [x]
deoverlap (((s, f), x):((s', f'), x'):xs) | f > s' && f < f' = ((s, s'), x):deoverlap (((s', f'), x'):xs)
                                          | f > f' = ((s, s'), x):deoverlap (((s', f'), x'):((f', f), x):xs)
                                          | otherwise = ((s, f), x):deoverlap (((s', f'), x'):xs)

blobify :: Eq a => [((Int, Int), a)] -> [((Int, Int), a)] -> ([((Int, Int), a)], [((Int, Int), a)])
blobify a b = recompare (go a b, go b a) where
    go (((s, f), x):rs@(((_, f'), x'):xs)) ys | present x x' ys = go (((s, f'), x'):xs) ys
                                              | otherwise = ((s, f), x):go rs ys
    go x _ = x
    present _ _ [] = False
    present _ _ [_] = False
    present x x' ((_, y):rs@((_, y'):_)) | x == y && x' == y' = True | otherwise = present x x' rs
    recompare (ps, ps') = (filter (\(_, h) -> helem h ps') ps, filter (\(_, h) -> helem h ps) ps')
    helem _ [] = False
    helem h ((_, h'):xs) | h == h' = True | otherwise = helem h xs

splitInto :: T.Text -> [((Int, Int), a)] -> [Colored a]
splitInto = go 0 where
    go _ s [] | T.null s = []
              | otherwise = [Uncolored s]
    go off s (((sp, ep), x):ps) = if T.null preTok then Colored x tok:go ep postTok ps else Uncolored preTok:Colored x tok:go ep postTok ps
        where (preTok, preTokRest) = T.splitAt (sp - off) s
              (tok, postTok) = T.splitAt (ep - sp) preTokRest

toPosList :: Show a => T.Text -> [Tagged a] -> [((Int, Int), a)]
toPosList s p = deoverlap . sortBy (\a b -> compare (fst a) (fst b)) $ fmap convertPos p where
    ls = T.lines s
    convertPos :: Tagged a -> ((Int, Int), a)
    convertPos (Tagged x tp) = ((spos, epos), x) where
        spos = lineColToAbs (fromIntegral . unPos $ startLine tp) (fromIntegral . unPos $ startCol tp)
        epos = lineColToAbs (fromIntegral . unPos $ endLine tp) (fromIntegral . unPos $ endCol tp)
    lineColToAbs :: Int -> Int -> Int
    lineColToAbs l c = c + (l - 2) + sum (T.length <$> take (l - 1) ls)

renderBoth :: (Show a, Eq a) => FilePath -> (Fingerprints, a) -> (Fingerprints, a) -> IO (H.Html, H.Html)
renderBoth dir (fp, t) (fp', t') = do
        s <- T.IO.readFile (dir </> sq t)
        s' <- T.IO.readFile (dir </> sq t')
        let es = T.replace "\t" "        " s
            es' = T.replace "\t" "        " s'
            (p, p') = (toPosList es fp, toPosList es' fp')
        return (mconcat . fmap colorize $ splitInto es p, mconcat . fmap colorize $ splitInto es' p')

renderCompare :: (Show a, Eq a) => FilePath -> (Double, (Fingerprints, a), (Fingerprints, a)) -> IO H.Html
renderCompare dir (m, g@(_, t), g'@(_, t')) = do
        (s, s') <- renderBoth dir g g'
        return $ mconcat
            [ H.h1 ! A.class_ "centered" $ H.toHtml (sq t ++ " vs. " ++ sq t' ++ ": " ++ showFFloat (Just 2) (m * 100) "% match")
            , H.div ! A.id "left" ! A.class_ "scrollable-pane" $ s
            , H.div ! A.id "right" ! A.class_ "scrollable-pane" $ s'
            ]
