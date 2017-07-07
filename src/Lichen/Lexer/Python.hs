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
         | AndAssign | OrAssign | XorAssign | LeftAssign | RightAssign
         deriving (Show, Read, Eq, Generic)
instance Hashable Tok

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "#") (L.skipBlockComment "\"\"\"" "\"\"\"")

reserved :: String -> Parser String
reserved = try . string

pyStrLit :: Parser String
pyStrLit = char '\'' *> manyTill L.charLiteral (char '\'')
        
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
       <|> wrap (show <$> (strLit <|> pyStrLit)) StringLiteral
       <|> wrap (show <$> (char 'b' *> (strLit <|> pyStrLit))) BytesLiteral
       <|> wrap (reserved "+") Plus
       <|> wrap (reserved "-") Minus
       <|> wrap (reserved "*") Asterisk
       <|> wrap (reserved "/") Slash
       <|> wrap (reserved "//") DoubleSlash
       <|> wrap (reserved "%") Percent
       <|> wrap (reserved "**") DoubleAsterisk
       <|> wrap (reserved "==") EqOp
       <|> wrap (reserved "!=") NeOp
       <|> wrap (reserved "<") LessThan
       <|> wrap (reserved ">") GreaterThan
       <|> wrap (reserved "<=") LeOp
       <|> wrap (reserved ">=") GeOp
       <|> wrap (reserved "&") Ampersand
       <|> wrap (reserved "|") Pipe
       <|> wrap (reserved "~") Tilde
       <|> wrap (reserved "^") Caret
       <|> wrap (reserved "<<") LeftOp
       <|> wrap (reserved ">>") RightOp
       <|> wrap (reserved "(") LeftParen
       <|> wrap (reserved ")") RightParen
       <|> wrap (reserved "[") LeftSquare
       <|> wrap (reserved "]") RightSquare
       <|> wrap (reserved "{") LeftCurly
       <|> wrap (reserved "}") RightCurly
       <|> wrap (reserved ".") Dot
       <|> wrap (reserved ",") Comma
       <|> wrap (reserved ":") Colon
       <|> wrap (reserved ";") Semicolon
       <|> wrap (reserved "@") At
       <|> wrap (reserved "=") Equal
       <|> wrap (reserved "+=") AddAssign
       <|> wrap (reserved "-=") SubAssign
       <|> wrap (reserved "*=") MulAssign
       <|> wrap (reserved "/=") DivAssign
       <|> wrap (reserved "//=") IntDivAssign
       <|> wrap (reserved "%=") ModAssign
       <|> wrap (reserved "**=") PowAssign
       <|> wrap (reserved "&=") AndAssign
       <|> wrap (reserved "|=") OrAssign
       <|> wrap (reserved "^=") XorAssign
       <|> wrap (reserved "<<=") LeftAssign
       <|> wrap (reserved ">>=") RightAssign

lex :: Lexer Tok
lex p d = case runParser (many (sc *> onetoken <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return t
