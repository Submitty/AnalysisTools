{-# LANGUAGE OverloadedStrings #-}

module Lichen.Count.Main where

import System.Directory

import Data.Semigroup ((<>))

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Count
import Lichen.Count.Counters

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> (counterChoice (counter dc) <$> (optional . strOption $ long "counter" <> short 'c' <> metavar "COUNTER" <> help "Counting method"))
               <*> optional (argument str (metavar "ELEMENT"))
               <*> many (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain c = do
        options <- liftIO $ execParser opts
        flip runConfigured options $ do
            config <- ask
            t <- case toCount config of Just t -> return t; Nothing -> throwError $ InvocationError "No countable element specified"
            ps <- liftIO . mapM canonicalizePath $ sourceFiles config
            counts <- lift $ mapM (runCounter (counter config) (language config) t) ps
            liftIO . print $ sum counts
    where opts = info (helper <*> parseOptions c) (fullDesc <> progDesc "Count occurences of a specific AST node" <> header "lichen-count-node - token counting")
