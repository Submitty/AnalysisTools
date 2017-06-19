module Lichen.Config where

import Lichen.Lexer

-- Configuration for the winnowing algorithm. Token sequences shorter than
-- noiseThreshold are considered noise, token sequences longer than
-- signalThreshold are always detected.
data WinnowConfig = WinnowConfig { signalThreshold :: Int, noiseThreshold :: Int }

-- Configuration for a given language. Should typically not need to be
-- modified, but can be overwritten in the case of unexpected instructor
-- use cases (non-typical file extensions, etc.).
data Language a = Language { extensions :: [FilePath], lexer :: Lexer a, winnowConfig :: WinnowConfig }

-- Overall configuration for plagiarism execution.
data PlagiarismConfig = PlagiarismConfig { }
