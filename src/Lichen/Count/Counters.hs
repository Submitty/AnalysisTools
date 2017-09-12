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
import Lichen.Util
import qualified Lichen.Parser as P

newtype Counter = Counter { runCounter :: Language -> String -> FilePath -> Erring Integer }
instance FromJSON Counter where
        parseJSON (String s) = pure $ counterChoice counterDummy (Just $ T.unpack s)
        parseJSON _ = pure counterDummy

counterDummy :: Counter
counterDummy = Counter $ \_ _ _ -> throwError $ InvocationError "Invalid counting method specified"

counterToken :: Counter
counterToken = Counter $ \(Language _ l _ readTok _) t p -> do
    ssrc <- liftIO $ readSafe (liftIO . BS.readFile) (throwError $ InvocationError "File not found") p
    src <- ssrc
    tokens <- l p src
    rt <- readTok t
    return . fromIntegral . length . filter (hash rt ==) . fmap (hash . tdata) $ tokens

parseCounter :: (T.Text -> P.Node -> Integer) -> Counter
parseCounter f = Counter $ \l t p -> do
    ssrc <- liftIO $ readSafe (liftIO . BS.readFile) (throwError $ InvocationError "File not found") p
    src <- ssrc
    tree <- parser l p src
    return $ f (T.pack t) tree

counterNode :: Counter
counterNode = parseCounter P.countTag

counterCall :: Counter
counterCall = parseCounter P.countCall

counterDepth :: Counter
counterDepth = parseCounter P.countDepth

counterChoice :: Counter -> Maybe String -> Counter
counterChoice d Nothing = d
counterChoice _ (Just "token") = counterToken
counterChoice _ (Just "node") = counterNode
counterChoice _ (Just "call") = counterCall
counterChoice _ (Just "func") = counterCall
counterChoice _ (Just "function") = counterCall
counterChoice _ (Just "depth") = counterDepth
counterChoice _ _ = counterDummy
