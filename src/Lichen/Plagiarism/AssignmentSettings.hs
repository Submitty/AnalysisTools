{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.AssignmentSettings where

import System.Directory

import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS

import Control.Monad.Except

import Lichen.Error
import Lichen.Config.Plagiarism

data VersionTime = VersionTime { version :: Int, time :: String }
instance FromJSON VersionTime where parseJSON = withObject "version_time" $ \o -> VersionTime <$> o .: "version" <*> o .: "time"
data AssignmentSettings = AssignmentSettings { activeVersion :: Int, history :: [VersionTime]}
instance FromJSON AssignmentSettings where parseJSON = withObject "assignment_settings" $ \o -> AssignmentSettings <$> o .: "active_version" <*> o .: "history"

getAssignmentSettings :: FilePath -> Plagiarism AssignmentSettings
getAssignmentSettings p = do
        b <- liftIO $ doesFileExist p
        if b
            then do c <- liftIO $ BS.readFile p
                    case eitherDecode c of Left e -> throwError . JSONDecodingError $ T.pack e
                                           Right t -> return t
            else throwError . JSONDecodingError . T.pack $ "Assignment settings file \"" ++ p ++ "\" not found."

getVersionTime :: AssignmentSettings -> Int -> String
getVersionTime a v = go (history a) where
    go (VersionTime v' s:xs) | v == v' = s
                             | otherwise = go xs
    go [] = ""

getStudentTime :: FilePath -> Plagiarism String
getStudentTime p = do
        as <- getAssignmentSettings p
        return . getVersionTime as $ activeVersion as
