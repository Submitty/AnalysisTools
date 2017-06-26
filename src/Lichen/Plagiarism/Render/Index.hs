module Lichen.Plagiarism.Render.Index where

import Data.Foldable
import Data.Monoid ((<>))

import Lucid

hs :: Show a => a -> Html ()
hs = toHtml . show

renderEntry :: Show a => (Double, a, a) -> Html ()
renderEntry (match, x, y) = tr_ (td_ (hs match) <> td_ (hs x) <> td_ (hs y))

renderTable :: Show a => [(Double, a, a)] -> Html ()
renderTable = traverse_ renderEntry
