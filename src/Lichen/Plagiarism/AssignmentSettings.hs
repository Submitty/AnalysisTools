{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.AssignmentSettings where

import System.Directory

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

import Control.Monad.Except

import Lichen.Error
import Lichen.Config.Plagiarism

newtype AssignmentSettings = AssignmentSettings { activeVersion :: Int }
instance FromJSON AssignmentSettings where
        parseJSON = withObject "assignment_settings" $ \o -> AssignmentSettings <$> o .: "active_version"

getAssignmentSettings :: FilePath -> Plagiarism AssignmentSettings
getAssignmentSettings p = do
        b <- liftIO $ doesFileExist p
        if b
            then do c <- liftIO $ BS.readFile p
                    case eitherDecode c of Left e -> throwError . JSONDecodingError $ T.pack e
                                           Right t -> return t
            else throwError . JSONDecodingError . T.pack $ "Assignment settings file \"" ++ p ++ "\" not found."
