{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.AssignmentSettings where

import qualified Data.Text as T

import Control.Monad.Except

import Text.JSON

import Lichen.Error
import Lichen.Config.Plagiarism

newtype AssignmentSettings = AssignmentSettings { activeVersion :: Int }

getAssignmentSettings :: FilePath -> Plagiarism AssignmentSettings
getAssignmentSettings p = do
        contents <- liftIO $ readFile p
        alist <- case resultToEither $ decodeStrict contents of
            Left e -> throwError . JSONDecodingError $ T.pack e
            Right r -> case r of JSObject obj -> return $ fromJSObject obj
                                 _ -> throwError $ JSONDecodingError "Invalid user assignment settings"
        case lookup "active_version" alist of
            Just x -> case resultToEither $ readJSON x of
                          Left e -> throwError . JSONDecodingError $ T.pack e
                          Right r -> return $ AssignmentSettings r
            Nothing -> throwError $ JSONDecodingError "active_version not present in user assignment settings"
            
