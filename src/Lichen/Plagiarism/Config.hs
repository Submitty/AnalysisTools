{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Lichen.Plagiarism.Config where

import Data.Maybe
import Data.Aeson
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H

import Lichen.Config
import Lichen.Languages

newtype PathGenerator = PathGenerator { runPathGenerator :: Config -> String -> String -> H.AttributeValue }
instance FromJSON PathGenerator where
        parseJSON (String s) = pure $ generatePathChoice generatePathSubmitty (Just $ T.unpack s)
        parseJSON _ = pure generatePathSubmitty

generatePathStatic :: PathGenerator
generatePathStatic = PathGenerator $ \_ x y -> H.stringValue $ "compare/" ++ x ++ "_" ++ y ++ ".html"

generatePathSubmitty :: PathGenerator
generatePathSubmitty = PathGenerator $ \c x y -> H.stringValue $ mconcat [ "index.php?semester=", submittySemester c
                                                                         , "&course=", submittyCourse c
                                                                         , "&assignment=", submittyAssignment c
                                                                         , "&component=admin&page=plagiarism&action=compare&studenta=", x, "&studentb=", y ]

generatePathChoice :: PathGenerator -> Maybe String -> PathGenerator
generatePathChoice d Nothing = d
generatePathChoice _ (Just "static") = generatePathStatic
generatePathChoice _ _ = generatePathSubmitty

data Config = Config
            { dataDir :: FilePath
            , concatDir :: FilePath
            , highlightDir :: FilePath
            , reportDir :: FilePath
            , reportTitle :: T.Text
            , language :: Language
            , topMatches :: Int
            , pathGenerator :: PathGenerator
            , submittySemester :: FilePath
            , submittyCourse :: FilePath
            , submittyAssignment :: FilePath
            , allVersions :: Bool
            , sharedThreshold :: Float
            , sourceDir :: Maybe FilePath
            , pastDirs :: [FilePath]
            }
instance FromJSON Config where
        parseJSON = withObject "config_plagiarism" $ \o -> do
            dataDir <- fromMaybe (dataDir defaultConfig) <$> o .:? "data_dir"
            concatDir <- fromMaybe (concatDir defaultConfig) <$> o .:? "concat_dir"
            highlightDir <- fromMaybe (highlightDir defaultConfig) <$> o .:? "highlight_dir"
            reportDir <- fromMaybe (reportDir defaultConfig) <$> o .:? "report_dir"
            reportTitle <- fromMaybe (reportTitle defaultConfig) <$> o .:? "report_title"
            language <- fromMaybe (language defaultConfig) <$> o .:? "language"
            topMatches <- fromMaybe (topMatches defaultConfig) <$> o .:? "top_matches"
            pathGenerator <- fromMaybe (pathGenerator defaultConfig) <$> o .:? "path_generator"
            submittySemester <- fromMaybe (submittySemester defaultConfig) <$> o .:? "submitty_semester"
            submittyCourse <- fromMaybe (submittyCourse defaultConfig) <$> o .:? "submitty_course"
            submittyAssignment <- fromMaybe (submittyAssignment defaultConfig) <$> o .:? "submitty_assignment"
            allVersions <- fromMaybe (allVersions defaultConfig) <$> o .:? "all_versions"
            sharedThreshold <- fromMaybe (sharedThreshold defaultConfig) <$> o .:? "shared_threshold"
            sourceDir <- fromMaybe (sourceDir defaultConfig) <$> o .:? "source_dir"
            pastDirs <- fromMaybe (pastDirs defaultConfig) <$> o .:? "past_dirs"
            return Config{..}

defaultConfig :: Config
defaultConfig = Config { dataDir = "plagiarism"
                       , concatDir = "concatenated"
                       , highlightDir = "highlighted"
                       , reportDir = "report"
                       , reportTitle = "Plagiarism Detection"
                       , language = langDummy
                       , topMatches = 100
                       , pathGenerator = generatePathSubmitty
                       , submittySemester = "invalid"
                       , submittyCourse = "invalid"
                       , submittyAssignment = "invalid"
                       , allVersions = False
                       , sharedThreshold = 0.5
                       , sourceDir = Nothing
                       , pastDirs = []
                       }

type Plagiarism = Configured Config
