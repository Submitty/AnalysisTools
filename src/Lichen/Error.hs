{-# LANGUAGE OverloadedStrings #-}

module Lichen.Error where

import System.IO (stderr, hPutStrLn)

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS

import Text.Megaparsec (Token, Dec)
import Text.Megaparsec.Error (ParseError, parseErrorPretty)

import Control.Monad.Except (ExceptT)

import Lichen.Util

type Erring = ExceptT LichenError IO

data LichenError = LexError (ParseError (Token BS.ByteString) Dec)
                 | ParseError T.Text
                 | InvalidTokenError T.Text
                 | InvocationError T.Text
                 | JSONDecodingError T.Text
                 deriving Show

printError :: LichenError -> IO ()
printError (LexError e) = err $ T.IO.hPutStrLn stderr "Lexer error: " >> hPutStrLn stderr (parseErrorPretty e)
printError (ParseError t) = err $ T.IO.hPutStrLn stderr ("Parser error: " <> t)
printError (InvalidTokenError t) = err $ T.IO.hPutStrLn stderr ("Invalid token error: " <> t)
printError (InvocationError t) = err $ T.IO.hPutStrLn stderr ("Invocation error: " <> t)
printError (JSONDecodingError t) = err $ T.IO.hPutStrLn stderr ("JSON decoding error: " <> t)
