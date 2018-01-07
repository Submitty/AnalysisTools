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
       <|> wrap (quote <$> strLit) StringLiteral
       <|> wrap (quote <$> charLit) CharLiteral
       <|> wrap (operator ">>=") RightAssign
       <|> wrap (operator "<<=") LeftAssign
       <|> wrap (operator ">>>") ThreeRight
       <|> wrap (operator "+=") AddAssign
       <|> wrap (operator "-=") SubAssign
       <|> wrap (operator "*=") MulAssign
       <|> wrap (operator "/=") DivAssign
       <|> wrap (operator "%=") ModAssign
       <|> wrap (operator "&=") AndAssign
       <|> wrap (operator "^=") XorAssign
       <|> wrap (operator "|=") OrAssign
       <|> wrap (operator ">>") RightOp
       <|> wrap (operator "<<") LeftOp
       <|> wrap (operator "++") IncOp
       <|> wrap (operator "--") DecOp
       <|> wrap (operator "&&") AndOp
       <|> wrap (operator "||") OrOp
       <|> wrap (operator "<=") LeOp
       <|> wrap (operator ">=") GeOp
       <|> wrap (operator "==") EqOp
       <|> wrap (operator "!=") NeOp
       <|> wrap (operator ";") Semicolon
       <|> wrap (operator "{") LeftCurly
       <|> wrap (operator "}") RightCurly
       <|> wrap (operator ",") Comma
       <|> wrap (operator ":") Colon
       <|> wrap (operator "=") Equal
       <|> wrap (operator "(") LeftParen
       <|> wrap (operator ")") RightParen
       <|> wrap (operator "[") LeftSquare
       <|> wrap (operator "]") RightSquare
       <|> wrap (operator ".") Dot
       <|> wrap (operator "&") Ampersand
       <|> wrap (operator "!") Exclamation
       <|> wrap (operator "~") Tilde
       <|> wrap (operator "-") Minus
       <|> wrap (operator "+") Plus
       <|> wrap (operator "*") Asterisk
       <|> wrap (operator "/") Slash
       <|> wrap (operator "%") Percent
       <|> wrap (operator "<") LessThan
       <|> wrap (operator ">") GreaterThan
       <|> wrap (operator "^") Caret
       <|> wrap (operator "|") Pipe
       <|> wrap (operator "?") Question
       <|> wrap ((:[]) <$> anyChar) Unknown
        
lex :: Lexer Tok
lex p d = case runParser (many (sc *> onetoken <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return t
