{-# LANGUAGE OverloadedStrings #-}

module Lichen.Count.Main where

import System.Directory

import Data.Hashable
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad.Reader
import Control.Monad.Except

import Options.Applicative

import Lichen.Error
import Lichen.Config
import Lichen.Config.Languages
import Lichen.Config.Count
import qualified Lichen.Parser as P

countToken :: Language -> String -> FilePath -> Erring Integer
countToken (Language _ l _ readTok _) t p = do
        src <- liftIO $ BS.readFile p
        tokens <- l p src
        return . fromIntegral . length . filter (hash (readTok t) ==) . fmap hash $ tokens

countNode :: Language -> String -> FilePath -> Erring Integer
countNode l t p = do
        src <- liftIO $ BS.readFile p
        tree <- parser l p src
        return $ P.countTag (T.pack t) tree

countCall :: Language -> String -> FilePath -> Erring Integer
countCall l t p = do
        src <- liftIO $ BS.readFile p
        tree <- parser l p src
        return $ P.countCall (T.pack t) tree

dispatchCount :: String -> Language -> String -> FilePath -> Erring Integer
dispatchCount "token" = countToken
dispatchCount "node" = countNode
dispatchCount "call" = countCall
dispatchCount "function" = countCall
dispatchCount _ = counterDummy

parseOptions :: Config -> Parser Config
parseOptions dc = Config
               <$> (languageChoice (language dc) <$> (optional . strOption $ long "language" <> short 'l' <> metavar "LANG" <> help "Language of student code"))
               <*> fmap dispatchCount (argument str (metavar "COUNTER"))
               <*> optional (argument str (metavar "ELEMENT"))
               <*> many (argument str (metavar "SOURCE"))

realMain :: Config -> IO ()
realMain c = do
        options <- liftIO $ execParser opts
        flip runConfigured options $ do
            config <- ask
            t <- case toCount config of Just t -> return t; Nothing -> throwError $ InvocationError "No countable element specified"
            ps <- liftIO . mapM canonicalizePath $ sourceFiles config
            counts <- lift $ mapM (method config (language config) t) ps
            liftIO . print $ sum counts
    where opts = info (helper <*> parseOptions c) (fullDesc <> progDesc "Count occurences of a specific AST node" <> header "lichen-count-node - token counting")
