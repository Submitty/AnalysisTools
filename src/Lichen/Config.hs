{-# LANGUAGE GADTs #-}

module Lichen.Config where

import Data.Hashable

import Control.Monad.Reader

import Lichen.Lexer

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
-- Note: as of this writing (6/2017), the following line breaks HLint. This
-- is an HLint bug, and can safely be ignored.
data Language where Language :: Hashable a => { exts :: [FilePath], lexer :: Lexer a, winnowConfig :: WinnowConfig } -> Language

-- Overall configuration for plagiarism execution.
data PlagiarismConfig = PlagiarismConfig
                      { dataDir :: FilePath
                      , concatDir :: FilePath
                      , highlightDir :: FilePath
                      , language :: Language
                      }

type Plagiarism = ReaderT PlagiarismConfig IO

defaultPlagiarismConfig :: PlagiarismConfig
defaultPlagiarismConfig = PlagiarismConfig { dataDir = ".lichen"
                                           , concatDir = "concatenated"
                                           , highlightDir = "highlighted"
                                           , language = undefined
                                           }
