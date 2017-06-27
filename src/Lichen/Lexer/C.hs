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
         | Question
         deriving (Show, Read, Eq, Generic)
instance Hashable Tok

sc :: Parser ()
sc = L.space (void spaceChar) (L.skipLineComment "//" <|> L.skipLineComment "#") (L.skipBlockComment "/*" "*/")

reserved :: String -> Parser ()
reserved = void . try . string

onetoken :: Parser Tok
onetoken = (reserved "auto" *> pure Auto)
       <|> (reserved "break" *> pure Break)
       <|> (reserved "case" *> pure Case)
       <|> (reserved "char" *> pure Char)
       <|> (reserved "const" *> pure Const)
       <|> (reserved "continue" *> pure Continue)
       <|> (reserved "default" *> pure Default)
       <|> (reserved "do" *> pure Do)
       <|> (reserved "double" *> pure Double)
       <|> (reserved "else" *> pure Else)
       <|> (reserved "enum" *> pure Enum)
       <|> (reserved "extern" *> pure Extern)
       <|> (reserved "float" *> pure Float)
       <|> (reserved "for" *> pure For)
       <|> (reserved "goto" *> pure Goto)
       <|> (reserved "if" *> pure If)
       <|> (reserved "inline" *> pure Inline)
       <|> (reserved "int" *> pure Int)
       <|> (reserved "long" *> pure Long)
       <|> (reserved "register" *> pure Register)
       <|> (reserved "restrict" *> pure Restrict)
       <|> (reserved "return" *> pure Return)
       <|> (reserved "short" *> pure Short)
       <|> (reserved "signed" *> pure Signed)
       <|> (reserved "sizeof" *> pure Sizeof)
       <|> (reserved "static" *> pure Static)
       <|> (reserved "struct" *> pure Struct)
       <|> (reserved "switch" *> pure Switch)
       <|> (reserved "typedef" *> pure Typedef)
       <|> (reserved "union" *> pure Union)
       <|> (reserved "unsigned" *> pure Unsigned)
       <|> (reserved "void" *> pure Void)
       <|> (reserved "volatile" *> pure Volatile)
       <|> (reserved "while" *> pure While)
       <|> (reserved "_Alignas" *> pure AlignAs)
       <|> (reserved "_Alignof" *> pure AlignOf)
       <|> (reserved "_Atomic" *> pure Atomic)
       <|> (reserved "_Bool" *> pure Bool)
       <|> (reserved "_Complex" *> pure Complex)
       <|> (reserved "_Generic" *> pure Generic)
       <|> (reserved "_Imaginary" *> pure Imaginary)
       <|> (reserved "_Noreturn" *> pure NoReturn)
       <|> (reserved "_Static_assert" *> pure StaticAssert)
       <|> (reserved "_Thread_local" *> pure ThreadLocal)
       <|> (reserved "__func__" *> pure FuncName)
       <|> (ident *> pure Identifier)
       <|> (L.integer *> pure IntegerLiteral)
       <|> (L.float *> pure FloatLiteral)
       <|> (strLit *> pure StringLiteral)
       <|> (charLit *> pure CharLiteral)
       <|> (reserved "..." *> pure Ellipsis)
       <|> (reserved ">>=" *> pure RightAssign)
       <|> (reserved "<<=" *> pure LeftAssign)
       <|> (reserved "+=" *> pure AddAssign)
       <|> (reserved "-=" *> pure SubAssign)
       <|> (reserved "*=" *> pure MulAssign)
       <|> (reserved "/=" *> pure DivAssign)
       <|> (reserved "%=" *> pure ModAssign)
       <|> (reserved "&=" *> pure AndAssign)
       <|> (reserved "^=" *> pure XorAssign)
       <|> (reserved "|=" *> pure OrAssign)
       <|> (reserved ">>" *> pure RightOp)
       <|> (reserved "<<" *> pure LeftOp)
       <|> (reserved "++" *> pure IncOp)
       <|> (reserved "--" *> pure DecOp)
       <|> (reserved "->" *> pure PtrOp)
       <|> (reserved "&&" *> pure AndOp)
       <|> (reserved "||" *> pure OrOp)
       <|> (reserved "<=" *> pure LeOp)
       <|> (reserved ">=" *> pure GeOp)
       <|> (reserved "==" *> pure EqOp)
       <|> (reserved "!=" *> pure NeOp)
       <|> (reserved ";" *> pure Semicolon)
       <|> ((reserved "{" <|> reserved "<%") *> pure LeftCurly)
       <|> ((reserved "}" <|> reserved "%>") *> pure RightCurly)
       <|> (reserved "," *> pure Comma)
       <|> (reserved ":" *> pure Colon)
       <|> (reserved "=" *> pure Equal)
       <|> (reserved "(" *> pure LeftParen)
       <|> (reserved ")" *> pure RightParen)
       <|> ((reserved "[" <|> reserved "<:") *> pure LeftSquare)
       <|> ((reserved "]" <|> reserved ":>") *> pure RightSquare)
       <|> (reserved "." *> pure Dot)
       <|> (reserved "&" *> pure Ampersand)
       <|> (reserved "!" *> pure Tilde)
       <|> (reserved "-" *> pure Minus)
       <|> (reserved "+" *> pure Plus)
       <|> (reserved "*" *> pure Asterisk)
       <|> (reserved "/" *> pure Slash)
       <|> (reserved "%" *> pure Percent)
       <|> (reserved "<" *> pure LessThan)
       <|> (reserved ">" *> pure GreaterThan)
       <|> (reserved "^" *> pure Caret)
       <|> (reserved "|" *> pure Pipe)
       <|> (reserved "?" *> pure Question)
        
lex :: Lexer Tok
lex p d = case runParser (many (sc *> onetoken <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return t
