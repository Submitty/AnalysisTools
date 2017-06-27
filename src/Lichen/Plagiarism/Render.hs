module Lichen.Plagiarism.Render where

import qualified Text.Blaze.Html5 as H

hs :: Show a => a -> H.Html
hs = H.toHtml . show
