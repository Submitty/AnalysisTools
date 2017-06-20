module Lichen.Config.Languages where

import Lichen.Config

import qualified Lichen.Lexer.C as C
import qualified Lichen.Lexer.Python as Python

langC = Language [".c", ".h", ".cpp", ".hpp", ".C", ".H", ".cc"] C.lex (WinnowConfig 9 5)
configC = defaultPlagiarismConfig { language = langC }

langPython = Language [".py"] Python.lex (WinnowConfig 9 5)
configPython = defaultPlagiarismConfig { language = langPython }
