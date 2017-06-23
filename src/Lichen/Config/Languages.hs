{-# LANGUAGE OverloadedStrings, GADTs #-}

module Lichen.Config.Languages where

import Data.Hashable

import Control.Monad.Except

import Lichen.Error
import Lichen.Lexer
import Lichen.Parser
import qualified Lichen.Lexer.C as C
import qualified Lichen.Lexer.Python as Python
import qualified Lichen.Parser.Python as Python

-- Configuration for the winnowing algorithm. Token sequences shorter than
-- noiseThreshold are considered noise, token sequences longer than
-- signalThreshold are always detected.
data WinnowConfig = WinnowConfig
                  { signalThreshold :: Int
                  , noiseThreshold :: Int
                  }

-- Configuration for a given language. Should typically not need to be
-- modified, but can be overwritten in the case of unexpected instructor
-- use cases (non-typical file extensions, etc.).
-- Note: as of this writing (6/2017), the following lines break HLint. This
-- is an HLint bug, and can safely be ignored.
data Language where
        Language :: (Hashable a, Show a) => { exts :: [FilePath]
                                            , lexer :: Lexer a
                                            , winnowConfig :: WinnowConfig
                                            , readToken :: String -> a
                                            , parser :: Parser Node
                                            } -> Language

dummy :: a -> b -> Erring c
dummy _ _ = throwError $ InvocationError "Specified analysis method is undefined for language"

langDummy :: Language
langDummy = Language [] dummy (WinnowConfig 0 0) (const ()) dummy

readC :: String -> C.Tok
readC = read

langC :: Language
langC = Language [".c", ".h", ".cpp", ".hpp", ".C", ".H", ".cc"] C.lex (WinnowConfig 9 5) readC dummy

readPython :: String -> Python.Tok
readPython = read

langPython :: Language
langPython = Language [".py"] Python.lex (WinnowConfig 9 5) readPython Python.parse

languageChoice :: Language -> Maybe String -> Language
languageChoice d Nothing = d
languageChoice _ (Just "C") = langC
languageChoice _ (Just "c") = langC
languageChoice _ (Just "Python") = langPython
languageChoice _ (Just "python") = langPython
languageChoice _ (Just "py") = langPython
languageChoice _ _ = langDummy
