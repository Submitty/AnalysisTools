{-# LANGUAGE DeriveGeneric #-}

module Lichen.Lexer.C where

import GHC.Generics (Generic)

import Control.Monad
import Control.Monad.Except

import Data.Hashable

import Text.Megaparsec
import Text.Megaparsec.ByteString
import qualified Text.Megaparsec.Lexer as L

import Lichen.Error
import Lichen.Lexer

data Tok = Auto | Break | Case | Char | Const | Continue | Default | Do
         | Double | Else | Enum | Extern | Float | For | Goto | If | Inline
         | Int | Long | Register | Restrict | Return | Short | Signed | Sizeof
         | Static | Struct | Switch | Typedef | Union | Unsigned | Void
         | Volatile | While | AlignAs | AlignOf | Atomic | Bool | Complex
         | Generic | Imaginary | NoReturn | StaticAssert | ThreadLocal
         | FuncName | Identifier | IntegerLiteral | FloatLiteral | StringLiteral
         | CharLiteral | Ellipsis | RightAssign | LeftAssign | AddAssign
         | SubAssign | MulAssign | DivAssign | ModAssign | AndAssign | XorAssign
         | OrAssign | RightOp | LeftOp | IncOp | DecOp | PtrOp | AndOp | OrOp
         | LeOp | GeOp | EqOp | NeOp | Semicolon | LeftCurly | RightCurly
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
onetoken = wrap (reserved "auto") Auto
       <|> wrap (reserved "break") Break
       <|> wrap (reserved "case") Case
       <|> wrap (reserved "char") Char
       <|> wrap (reserved "const") Const
       <|> wrap (reserved "continue") Continue
       <|> wrap (reserved "default") Default
       <|> wrap (reserved "do") Do
       <|> wrap (reserved "double") Double
       <|> wrap (reserved "else") Else
       <|> wrap (reserved "enum") Enum
       <|> wrap (reserved "extern") Extern
       <|> wrap (reserved "float") Float
       <|> wrap (reserved "for") For
       <|> wrap (reserved "goto") Goto
       <|> wrap (reserved "if") If
       <|> wrap (reserved "inline") Inline
       <|> wrap (reserved "int") Int
       <|> wrap (reserved "long") Long
       <|> wrap (reserved "register") Register
       <|> wrap (reserved "restrict") Restrict
       <|> wrap (reserved "return") Return
       <|> wrap (reserved "short") Short
       <|> wrap (reserved "signed") Signed
       <|> wrap (reserved "sizeof") Sizeof
       <|> wrap (reserved "static") Static
       <|> wrap (reserved "struct") Struct
       <|> wrap (reserved "switch") Switch
       <|> wrap (reserved "typedef") Typedef
       <|> wrap (reserved "union") Union
       <|> wrap (reserved "unsigned") Unsigned
       <|> wrap (reserved "void") Void
       <|> wrap (reserved "volatile") Volatile
       <|> wrap (reserved "while") While
       <|> wrap (reserved "_Alignas") AlignAs
       <|> wrap (reserved "_Alignof") AlignOf
       <|> wrap (reserved "_Atomic") Atomic
       <|> wrap (reserved "_Bool") Bool
       <|> wrap (reserved "_Complex") Complex
       <|> wrap (reserved "_Generic") Generic
       <|> wrap (reserved "_Imaginary") Imaginary
       <|> wrap (reserved "_Noreturn") NoReturn
       <|> wrap (reserved "_Static_assert") StaticAssert
       <|> wrap (reserved "_Thread_local") ThreadLocal
       <|> wrap (reserved "__func__") FuncName
       <|> wrap ident Identifier
       <|> wrap (show <$> L.integer) IntegerLiteral
       <|> wrap (show <$> L.float) FloatLiteral
       <|> wrap (show <$> strLit) StringLiteral
       <|> wrap (show <$> charLit) CharLiteral
       <|> wrap (reserved "...") Ellipsis
       <|> wrap (reserved ">>=") RightAssign
       <|> wrap (reserved "<<=") LeftAssign
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
       <|> wrap (reserved "->") PtrOp
       <|> wrap (reserved "&&") AndOp
       <|> wrap (reserved "||") OrOp
       <|> wrap (reserved "<=") LeOp
       <|> wrap (reserved ">=") GeOp
       <|> wrap (reserved "==") EqOp
       <|> wrap (reserved "!=") NeOp
       <|> wrap (reserved ";") Semicolon
       <|> wrap (reserved "{" <|> reserved "<%") LeftCurly
       <|> wrap (reserved "}" <|> reserved "%>") RightCurly
       <|> wrap (reserved ",") Comma
       <|> wrap (reserved ":") Colon
       <|> wrap (reserved "=") Equal
       <|> wrap (reserved "(") LeftParen
       <|> wrap (reserved ")") RightParen
       <|> wrap (reserved "[" <|> reserved "<:") LeftSquare
       <|> wrap (reserved "]" <|> reserved ":>") RightSquare
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
