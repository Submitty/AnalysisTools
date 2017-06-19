{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.AssignmentSettings where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS

import Lichen.Config

newtype AssignmentSettings = AssignmentSettings { activeVersion :: Int }
instance FromJSON AssignmentSettings where
        parseJSON = withObject "assignment_settings"
                    $ \o -> AssignmentSettings <$> o .: "active_version"

getAssignmentSettings :: FilePath -> IO (Either String AssignmentSettings)
getAssignmentSettings = fmap eitherDecode . BS.readFile
