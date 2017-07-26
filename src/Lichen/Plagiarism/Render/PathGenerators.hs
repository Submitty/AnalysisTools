{-# LANGUAGE OverloadedStrings #-}

module Lichen.Plagiarism.Render.PathGenerators where

import Data.Aeson
import qualified Data.Text as T

import qualified Text.Blaze.Html5 as H

newtype PathGenerator = PathGenerator { runPathGenerator :: String -> String -> H.AttributeValue }
instance FromJSON PathGenerator where
        parseJSON (String s) = pure $ generatePathChoice generatePathSubmitty (Just $ T.unpack s)
        parseJSON _ = pure generatePathSubmitty

generatePathStatic :: PathGenerator
generatePathStatic = PathGenerator $ \x y -> H.stringValue $ "compare/" ++ x ++ "_" ++ y ++ ".html"

generatePathSubmitty :: PathGenerator
generatePathSubmitty = PathGenerator $ \x y -> H.stringValue $ "&page=plagiarism&studenta=" ++ x ++ "&studentb=" ++ y

generatePathChoice :: PathGenerator -> Maybe String -> PathGenerator
generatePathChoice d Nothing = d
generatePathChoice _ (Just "static") = generatePathStatic
generatePathChoice _ _ = generatePathSubmitty
