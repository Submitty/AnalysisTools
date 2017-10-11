{-# LANGUAGE DeriveGeneric #-}

module Lichen.Lexer.Java where

import GHC.Generics (Generic)

import Control.Monad
import Control.Monad.Except

import Data.Hashable

import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

import Lichen.Error
import Lichen.Lexer

data Tok = Abstract | Assert | Boolean | Break | Byte | Case | Catch | Char
         | Class | Const | Continue | Default | Do | Double | Else | Extends
         | Finally | Float | For | Goto | If | Implements | Import | InstanceOf
         | Int | Interface | Long | Native | New | Package | Private
         | Protected | Public | Return | Static | StrictFP | Super
         | Synchronized | Switch |This | Throw | Throws | Transient | Try | Void
         | Volatile | While | Identifier | IntegerLiteral | FloatLiteral
         | StringLiteral | CharLiteral | RightAssign | LeftAssign | AddAssign
         | SubAssign | MulAssign | DivAssign | ModAssign | AndAssign | XorAssign
         | OrAssign | RightOp | LeftOp | ThreeRight | IncOp | DecOp | AndOp
         | OrOp | LeOp | GeOp | EqOp | NeOp | Semicolon | LeftCurly | RightCurly
         | Comma | Colon | Equal | LeftParen | RightParen | LeftSquare
         | RightSquare | Dot | Ampersand | Exclamation | Tilde | Minus | Plus
         | Asterisk | Slash | Percent | LessThan | GreaterThan | Caret | Pipe
         | Question | Unknown
         deriving (Show, Read, Eq, Generic)
instance Hashable Tok

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "//" <|> L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

reserved :: String -> Parser String
reserved = try . string

onetoken :: Parser (Tagged Tok)
onetoken = wrap (reserved "abstract") Abstract
       <|> wrap (reserved "assert") Assert
       <|> wrap (reserved "boolean") Boolean
       <|> wrap (reserved "break") Break
       <|> wrap (reserved "byte") Byte
       <|> wrap (reserved "case") Case
       <|> wrap (reserved "catch") Catch
       <|> wrap (reserved "char") Char
       <|> wrap (reserved "class") Class
       <|> wrap (reserved "const") Const
       <|> wrap (reserved "continue") Continue
       <|> wrap (reserved "default") Default
       <|> wrap (reserved "do") Do
       <|> wrap (reserved "double") Double
       <|> wrap (reserved "else") Else
       <|> wrap (reserved "extends") Extends
       <|> wrap (reserved "finally") Finally
       <|> wrap (reserved "float") Float
       <|> wrap (reserved "for") For
       <|> wrap (reserved "goto") Goto
       <|> wrap (reserved "if") If
       <|> wrap (reserved "implements") Implements
       <|> wrap (reserved "import") Import
       <|> wrap (reserved "instanceof") InstanceOf
       <|> wrap (reserved "int") Int
       <|> wrap (reserved "interface") Interface
       <|> wrap (reserved "long") Long
       <|> wrap (reserved "native") Native
       <|> wrap (reserved "new") New
       <|> wrap (reserved "package") Package
       <|> wrap (reserved "private") Private
       <|> wrap (reserved "protected") Protected
       <|> wrap (reserved "public") Public
       <|> wrap (reserved "return") Return
       <|> wrap (reserved "static") Static
       <|> wrap (reserved "strictfp") StrictFP
       <|> wrap (reserved "super") Super
       <|> wrap (reserved "synchronized") Synchronized
       <|> wrap (reserved "switch") Switch
       <|> wrap (reserved "this") This
       <|> wrap (reserved "throw") Throw
       <|> wrap (reserved "throws") Throws
       <|> wrap (reserved "transient") Transient
       <|> wrap (reserved "try") Try
       <|> wrap (reserved "void") Void
       <|> wrap (reserved "volatile") Volatile
       <|> wrap (reserved "while") While
       <|> wrap ident Identifier
       <|> wrap (show <$> L.integer) IntegerLiteral
       <|> wrap (show <$> L.float) FloatLiteral
       <|> wrap (show <$> strLit) StringLiteral
       <|> wrap (show <$> charLit) CharLiteral
       <|> wrap (reserved ">>=") RightAssign
       <|> wrap (reserved "<<=") LeftAssign
       <|> wrap (reserved ">>>") ThreeRight
       <|> wrap (reserved "+=") AddAssign
       <|> wrap (reserved "-=") SubAssign
       <|> wrap (reserved "*=") MulAssign
       <|> wrap (reserved "/=") DivAssign
       <|> wrap (reserved "%=") ModAssign
       <|> wrap (reserved "&=") AndAssign
       <|> wrap (reserved "^=") XorAssign
       <|> wrap (reserved "|=") OrAssign
       <|> wrap (reserved ">>") RightOp
       <|> wrap (reserved "<<") LeftOp
       <|> wrap (reserved "++") IncOp
       <|> wrap (reserved "--") DecOp
       <|> wrap (reserved "&&") AndOp
       <|> wrap (reserved "||") OrOp
       <|> wrap (reserved "<=") LeOp
       <|> wrap (reserved ">=") GeOp
       <|> wrap (reserved "==") EqOp
       <|> wrap (reserved "!=") NeOp
       <|> wrap (reserved ";") Semicolon
       <|> wrap (reserved "{") LeftCurly
       <|> wrap (reserved "}") RightCurly
       <|> wrap (reserved ",") Comma
       <|> wrap (reserved ":") Colon
       <|> wrap (reserved "=") Equal
       <|> wrap (reserved "(") LeftParen
       <|> wrap (reserved ")") RightParen
       <|> wrap (reserved "[") LeftSquare
       <|> wrap (reserved "]") RightSquare
       <|> wrap (reserved ".") Dot
       <|> wrap (reserved "&") Ampersand
       <|> wrap (reserved "!") Exclamation
       <|> wrap (reserved "~") Tilde
       <|> wrap (reserved "-") Minus
       <|> wrap (reserved "+") Plus
       <|> wrap (reserved "*") Asterisk
       <|> wrap (reserved "/") Slash
       <|> wrap (reserved "%") Percent
       <|> wrap (reserved "<") LessThan
       <|> wrap (reserved ">") GreaterThan
       <|> wrap (reserved "^") Caret
       <|> wrap (reserved "|") Pipe
       <|> wrap (reserved "?") Question
       <|> wrap ((:[]) <$> anyChar) Unknown
        
lex :: Lexer Tok
lex p d = case runParser (many (sc *> onetoken <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return t
