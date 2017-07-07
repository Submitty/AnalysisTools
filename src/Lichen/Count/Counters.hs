{-# LANGUAGE OverloadedStrings #-}

module Lichen.Count.Counters where

import Data.Hashable
import Data.Aeson
import qualified Data.Text as T
import qualified Data.ByteString as BS

import Control.Monad.Except

import Lichen.Error
import Lichen.Config.Languages
import Lichen.Lexer
import qualified Lichen.Parser as P

newtype Counter = Counter { runCounter :: Language -> String -> FilePath -> Erring Integer }
instance FromJSON Counter where
        parseJSON (String s) = pure $ counterChoice counterDummy (Just $ T.unpack s)
        parseJSON _ = pure counterDummy

counterDummy :: Counter
counterDummy = Counter $ \_ _ _ -> throwError $ InvocationError "Invalid counting method specified"

counterToken :: Counter
counterToken = Counter $ \(Language _ l _ readTok _) t p -> do
        src <- liftIO $ BS.readFile p
        tokens <- l p src
        rt <- readTok t
        return . fromIntegral . length . filter (hash rt ==) . fmap (hash . tdata) $ tokens

counterNode :: Counter
counterNode = Counter $ \l t p -> do
        src <- liftIO $ BS.readFile p
        tree <- parser l p src
        return $ P.countTag (T.pack t) tree

counterCall :: Counter
counterCall = Counter $ \l t p -> do
        src <- liftIO $ BS.readFile p
        tree <- parser l p src
        return $ P.countCall (T.pack t) tree

counterChoice :: Counter -> Maybe String -> Counter
counterChoice d Nothing = d
counterChoice _ (Just "token") = counterToken
counterChoice _ (Just "node") = counterNode
counterChoice _ (Just "call") = counterCall
counterChoice _ _ = counterDummy
