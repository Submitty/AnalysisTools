{-# LANGUAGE OverloadedStrings #-}

module Lichen.Error where

import System.IO (stderr)

import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.ByteString as BS

import Text.Megaparsec (Token, Dec)
import Text.Megaparsec.Error (ParseError, parseErrorPretty)

import Control.Monad.Except (ExceptT)

type Erring = ExceptT LichenError IO

data LichenError = LexError (ParseError (Token BS.ByteString) Dec)
                 | ParseError T.Text
                 | InvocationError T.Text
                 | JSONDecodingError T.Text
                 deriving Show

printError :: LichenError -> IO ()
printError (LexError e) = T.IO.hPutStrLn stderr "Lexer error: " >> putStrLn (parseErrorPretty e)
printError (ParseError t) = T.IO.hPutStrLn stderr ("Parser error: " <> t)
printError (InvocationError t) = T.IO.hPutStrLn stderr ("Invocation error: " <> t)
printError (JSONDecodingError t) = T.IO.hPutStrLn stderr ("JSON decoding error: " <> t)
