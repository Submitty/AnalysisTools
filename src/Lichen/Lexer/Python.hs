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

reserved :: String -> Parser ()
reserved = void . try . string

pyStrLit :: Parser String
pyStrLit = char '\'' *> manyTill L.charLiteral (char '\'')
        
onetoken :: Parser Tok
onetoken = (reserved "False" *> pure Lichen.Lexer.Python.False)
       <|> (reserved "None" *> pure None)
       <|> (reserved "True" *> pure Lichen.Lexer.Python.True)
       <|> (reserved "and" *> pure And)
       <|> (reserved "as" *> pure As)
       <|> (reserved "assert" *> pure Assert)
       <|> (reserved "break" *> pure Break)
       <|> (reserved "class" *> pure Class)
       <|> (reserved "continue" *> pure Continue)
       <|> (reserved "def" *> pure Def)
       <|> (reserved "del" *> pure Del)
       <|> (reserved "elif" *> pure Elif)
       <|> (reserved "else" *> pure Else)
       <|> (reserved "except" *> pure Except)
       <|> (reserved "finally" *> pure Finally)
       <|> (reserved "for" *> pure For)
       <|> (reserved "from" *> pure From)
       <|> (reserved "global" *> pure Global)
       <|> (reserved "if" *> pure If)
       <|> (reserved "import" *> pure Import)
       <|> (reserved "in" *> pure In)
       <|> (reserved "is" *> pure Is)
       <|> (reserved "lambda" *> pure Lambda)
       <|> (reserved "nonlocal" *> pure Nonlocal)
       <|> (reserved "not" *> pure Not)
       <|> (reserved "or" *> pure Or)
       <|> (reserved "pass" *> pure Pass)
       <|> (reserved "raise" *> pure Raise)
       <|> (reserved "return" *> pure Return)
       <|> (reserved "try" *> pure Try)
       <|> (reserved "while" *> pure While)
       <|> (reserved "with" *> pure With)
       <|> (reserved "yield" *> pure Yield)
       <|> (ident *> pure Identifier)
       <|> (L.integer *> pure IntegerLiteral)
       <|> (L.float *> pure FloatLiteral)
       <|> ((strLit <|> pyStrLit) *> pure StringLiteral)
       <|> (char 'b' *> (strLit <|> pyStrLit) *> pure BytesLiteral)
       <|> (reserved "+" *> pure Plus)
       <|> (reserved "-" *> pure Minus)
       <|> (reserved "*" *> pure Asterisk)
       <|> (reserved "/" *> pure Slash)
       <|> (reserved "//" *> pure DoubleSlash)
       <|> (reserved "%" *> pure Percent)
       <|> (reserved "**" *> pure DoubleAsterisk)
       <|> (reserved "==" *> pure EqOp)
       <|> (reserved "!=" *> pure NeOp)
       <|> (reserved "<" *> pure LessThan)
       <|> (reserved ">" *> pure GreaterThan)
       <|> (reserved "<=" *> pure LeOp)
       <|> (reserved ">=" *> pure GeOp)
       <|> (reserved "&" *> pure Ampersand)
       <|> (reserved "|" *> pure Pipe)
       <|> (reserved "~" *> pure Tilde)
       <|> (reserved "^" *> pure Caret)
       <|> (reserved "<<" *> pure LeftOp)
       <|> (reserved ">>" *> pure RightOp)
       <|> (reserved "(" *> pure LeftParen)
       <|> (reserved ")" *> pure RightParen)
       <|> (reserved "[" *> pure LeftSquare)
       <|> (reserved "]" *> pure RightSquare)
       <|> (reserved "{" *> pure LeftCurly)
       <|> (reserved "}" *> pure RightCurly)
       <|> (reserved "." *> pure Dot)
       <|> (reserved "," *> pure Comma)
       <|> (reserved ":" *> pure Colon)
       <|> (reserved ";" *> pure Semicolon)
       <|> (reserved "@" *> pure At)
       <|> (reserved "=" *> pure Equal)
       <|> (reserved "+=" *> pure AddAssign)
       <|> (reserved "-=" *> pure SubAssign)
       <|> (reserved "*=" *> pure MulAssign)
       <|> (reserved "/=" *> pure DivAssign)
       <|> (reserved "//=" *> pure IntDivAssign)
       <|> (reserved "%=" *> pure ModAssign)
       <|> (reserved "**=" *> pure PowAssign)
       <|> (reserved "&=" *> pure AndAssign)
       <|> (reserved "|=" *> pure OrAssign)
       <|> (reserved "^=" *> pure XorAssign)
       <|> (reserved "<<=" *> pure LeftAssign)
       <|> (reserved ">>=" *> pure RightAssign)

lex :: Lexer Tok
lex p d = case runParser (many (sc *> onetoken <* sc)) p d of
              Left e -> throwError $ LexError e
              Right t -> return t
