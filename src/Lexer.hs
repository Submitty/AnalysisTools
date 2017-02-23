{-# LANGUAGE OverloadedStrings #-}

module Lexer where

import System.Environment
import System.Process

import Data.Hashable
import Data.Monoid
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T.IO
import qualified Data.Text.Lazy.Read as T.R

import GHC.IO.Handle

import Language

type Token = (Int, Int)
type Session = (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)

lexerSession :: Language -> IO Session
lexerSession (Language l _ _) = do
        runpath <- getExecutablePath
        let pre = T.intercalate "/" $ reverse $ drop 2 $ reverse $ T.splitOn "/" $ T.pack runpath in
            createProcess $ (proc (T.unpack (pre <> "/lang/" <> l <> "/lex")) []) { std_in = CreatePipe, std_out = CreatePipe }

execute :: Session -> T.Text -> IO T.Text
execute s@(Just stdin, Just stdout, _, _) input = do
        T.IO.hPutStr stdin $ input <> "\n"
        hClose stdin
        T.IO.hGetContents stdout

runLexer :: Language -> T.Text -> IO [Token]
runLexer l input = do
        s <- lexerSession l
        out <- execute s input
        return $ pairing $ (fromIntegral . fst) <$> purify (T.R.decimal <$> T.words out)
    where purify [] = []
          purify (Left _:ls) = purify ls
          purify (Right i:ls) = i:purify ls
          pairing (t:l:ls) = (t, l):pairing ls
          pairing _ = []
