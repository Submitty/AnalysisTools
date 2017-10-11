{-# LANGUAGE OverloadedStrings, GADTs, DeriveGeneric, StandaloneDeriving #-}

module Lichen.Languages where

import GHC.Generics

import Data.Hashable
import Data.Aeson
import qualified Data.Text as T

import Control.Monad.Except

import Text.Read (readMaybe)

import Lichen.Error
import Lichen.Lexer
import Lichen.Parser
import qualified Lichen.Lexer.C as C
import qualified Lichen.Lexer.Python as Python
import qualified Lichen.Lexer.Java as Java
import qualified Lichen.Parser.Python as Python

-- Configuration for the winnowing algorithm. Token sequences shorter than
-- noiseThreshold are considered noise, token sequences longer than
-- signalThreshold are always detected.
data WinnowConfig = WinnowConfig
                  { signalThreshold :: Int
                  , noiseThreshold :: Int
                  } deriving Generic
instance FromJSON WinnowConfig

-- Configuration for a given language. Should typically not need to be
-- modified, but can be overwritten in the case of unexpected instructor
-- use cases (non-typical file extensions, etc.).
-- Note: as of this writing (6/2017), the following lines break HLint. This
-- is an HLint bug, and can safely be ignored.
data Language where
        Language :: (Hashable a, Show a) => { exts :: [FilePath]
                                            , winnowConfig :: WinnowConfig
                                            , readToken :: String -> Erring a
                                            , lexer :: Lexer a
                                            , parser :: Parser Node
                                            } -> Language
instance FromJSON Language where
        parseJSON (String s) = pure $ languageChoice langDummy (Just $ T.unpack s)
        parseJSON _ = pure langDummy

dummy :: T.Text -> a -> b -> Erring c
dummy t _ _ = throwError $ InvocationError t

smartRead :: Read a => String -> Erring a
smartRead s = case readMaybe s of Just t -> pure t
                                  Nothing -> throwError . InvalidTokenError $ T.pack s

langDummy :: Language
langDummy = Language [] (WinnowConfig 0 0) (const $ pure ()) (dummy "No valid language specified") (dummy "No valid language specified")

langC :: Language
langC = Language [".c", ".h", ".cpp", ".hpp", ".C", ".H", ".cc"] (WinnowConfig 16 9) (smartRead :: String -> Erring C.Tok) C.lex (dummy "The C tooling does not currently support the requested feature")

langPython :: Language
langPython = Language [".py"] (WinnowConfig 16 9) (smartRead :: String -> Erring Python.Tok) Python.lex Python.parse

langJava :: Language
langJava = Language [".java"] (WinnowConfig 16 9) (smartRead :: String -> Erring Java.Tok) Java.lex (dummy "The Java tooling does not currently support the requested feature")

languageChoice :: Language -> Maybe String -> Language
languageChoice d Nothing = d
languageChoice _ (Just "C") = langC
languageChoice _ (Just "c") = langC
languageChoice _ (Just "Python") = langPython
languageChoice _ (Just "python") = langPython
languageChoice _ (Just "py") = langPython
languageChoice _ (Just "java") = langJava
languageChoice _ (Just "Java") = langJava
languageChoice _ _ = langDummy
