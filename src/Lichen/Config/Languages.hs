module Lichen.Config.Languages where

import Lichen.Config

import qualified Lichen.Lexer.C as C

langC = Language ["c", "h", "cpp", "hpp", "C", "H", "cc"] C.lex (WinnowConfig 9 5)
configC = defaultPlagiarismConfig { language = langC }
