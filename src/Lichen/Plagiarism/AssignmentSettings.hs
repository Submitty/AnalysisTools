{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.AssignmentSettings where

import Lichen.Config.Plagiarism

newtype AssignmentSettings = AssignmentSettings { activeVersion :: Int }

getAssignmentSettings :: FilePath -> Plagiarism AssignmentSettings
getAssignmentSettings = undefined
