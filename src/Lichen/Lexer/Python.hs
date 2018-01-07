{-# LANGUAGE DeriveGeneric #-}

module Lichen.Lexer.Python where

import GHC.Generics (Generic)

import Control.Monad
import Control.Monad.Except

import Data.Hashable

import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

import Lichen.Error
import Lichen.Lexer

data Tok = False | None | True | And | As | Assert | Break | Class | Continue
         | Def | Del | Elif | Else | Except | Finally | For | From | Global | If
         | Import | In | Is | Lambda | Nonlocal | Not | Or | Pass | Raise
         | Return | Try | While | With | Yield | Identifier | IntegerLiteral
         | ImaginaryLiteral | FloatLiteral | StringLiteral | BytesLiteral
         | Plus | Minus | Asterisk | Slash | DoubleSlash | Percent
         | DoubleAsterisk | EqOp | NeOp | LessThan | GreaterThan | LeOp | GeOp
         | Ampersand | Pipe | Tilde | Caret | LeftOp | RightOp | LeftParen
         | RightParen | LeftSquare | RightSquare | LeftCurly | RightCurly | Dot
         | Comma | Colon | Semicolon | At | Equal | AddAssign | SubAssign
         | MulAssign | DivAssign | IntDivAssign | ModAssign | PowAssign
         | AndAssign | OrAssign | XorAssign | LeftAssign | RightAssign | Unknown
         deriving (Show, Read, Eq, Generic)
instance Hashable Tok

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "\"\"\"" "\"\"\"")

onetoken :: Parser (Tagged Tok)
onetoken = wrap (reserved "False") Lichen.Lexer.Python.False
       <|> wrap (reserved "None") None
       <|> wrap (reserved "True") Lichen.Lexer.Python.True
       <|> wrap (reserved "and") And
       <|> wrap (reserved "as") As
       <|> wrap (reserved "assert") Assert
       <|> wrap (reserved "break") Break
       <|> wrap (reserved "class") Class
       <|> wrap (reserved "continue") Continue
       <|> wrap (reserved "def") Def
       <|> wrap (reserved "del") Del
       <|> wrap (reserved "elif") Elif
       <|> wrap (reserved "else") Else
       <|> wrap (reserved "except") Except
       <|> wrap (reserved "finally") Finally
       <|> wrap (reserved "for") For
       <|> wrap (reserved "from") From
       <|> wrap (reserved "global") Global
       <|> wrap (reserved "if") If
       <|> wrap (reserved "import") Import
       <|> wrap (reserved "in") In
       <|> wrap (reserved "is") Is
       <|> wrap (reserved "lambda") Lambda
       <|> wrap (reserved "nonlocal") Nonlocal
       <|> wrap (reserved "not") Not
       <|> wrap (reserved "or") Or
       <|> wrap (reserved "pass") Pass
       <|> wrap (reserved "raise") Raise
       <|> wrap (reserved "return") Return
       <|> wrap (reserved "try") Try
       <|> wrap (reserved "while") While
       <|> wrap (reserved "with") With
       <|> wrap (reserved "yield") Yield
       <|> wrap ident Identifier
       <|> wrap (show <$> L.integer) IntegerLiteral
       <|> wrap (show <$> L.float) FloatLiteral
       <|> wrap (quote <$> (strLit <|> charLit)) StringLiteral
       <|> wrap (quote <$> (char 'b' *> (strLit <|> charLit))) BytesLiteral
       <|> wrap (operator "**=") PowAssign
       <|> wrap (operator "//=") IntDivAssign
       <|> wrap (operator "<<=") LeftAssign
       <|> wrap (operator ">>=") RightAssign
       <|> wrap (operator "+=") AddAssign
       <|> wrap (operator "-=") SubAssign
       <|> wrap (operator "*=") MulAssign
       <|> wrap (operator "/=") DivAssign
       <|> wrap (operator "%=") ModAssign
       <|> wrap (operator "&=") AndAssign
       <|> wrap (operator "|=") OrAssign
       <|> wrap (operator "^=") XorAssign
       <|> wrap (operator "**") DoubleAsterisk
       <|> wrap (operator "//") DoubleSlash
       <|> wrap (operator "==") EqOp
       <|> wrap (operator "!=") NeOp
       <|> wrap (operator "<=") LeOp
       <|> wrap (operator ">=") GeOp
       <|> wrap (operator "<<") LeftOp
       <|> wrap (operator ">>") RightOp
       <|> wrap (operator "+") Plus
       <|> wrap (operator "-") Minus
       <|> wrap (operator "*") Asterisk
       <|> wrap (operator "/") Slash
       <|> wrap (operator "%") Percent
       <|> wrap (operator "<") LessThan
       <|> wrap (operator ">") GreaterThan
       <|> wrap (operator "&") Ampersand
       <|> wrap (operator "|") Pipe
       <|> wrap (operator "~") Tilde
       <|> wrap (operator "^") Caret
       <|> wrap (operator "(") LeftParen
       <|> wrap (operator ")") RightParen
       <|> wrap (operator "[") LeftSquare
       <|> wrap (operator "]") RightSquare
       <|> wrap (operator "{") LeftCurly
       <|> wrap (operator "}") RightCurly
       <|> wrap (operator ".") Dot
       <|> wrap (operator ",") Comma
       <|> wrap (operator ":") Colon
       <|> wrap (operator ";") Semicolon
       <|> wrap (operator "@") At
       <|> wrap (operator "=") Equal
       <|> wrap ((:[]) <$> anyChar) Unknown

lex :: Lexer Tok
lex p d = case runParser (many (sc *> onetoken <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return t
