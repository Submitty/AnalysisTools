{-# LANGUAGE OverloadedStrings #-}

module Lichen.Parser.Python where

import Control.Monad
import Control.Monad.Except

import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS.C8

import Language.Python.Common.AST
import Language.Python.Version3.Parser

import Lichen.Error
import Lichen.Parser

(.%) :: (a -> b -> c) -> (c -> d -> e) -> (a -> b -> d -> e)
(.%) f g x y = g (f x y)

(+++) :: [a] -> [a] -> [a] -> [a]
(+++) = (++) .% (++)

(++++) :: [a] -> [a] -> [a] -> [a] -> [a]
(++++) x y = (+++) (x ++ y)

(+:) :: a -> a -> [a]
(+:) x y = x:[y]

possibly :: Maybe a -> [a]
possibly (Just x) = [x]
possibly Nothing = []

convertModule :: Module () -> Erring Node
convertModule (Module stmts) = Node [Tag "module"] <$> mapM convertStatement stmts

convertExpr :: Expr () -> Erring Node
convertExpr (Var name _) = Node [Tag "variable"] . (:[]) <$> convertIdent name
convertExpr (Int val lit _) = pure $ Node [Tag "literal", Tag "integer"] [DataInt val, DataStr $ T.pack lit]
convertExpr (Float val lit _) = pure $ Node [Tag "literal", Tag "float"] [DataFloat val, DataStr $ T.pack lit]
convertExpr (Imaginary val lit _) = pure $ Node [Tag "literal", Tag "imaginary"] [DataFloat val, DataStr $ T.pack lit]
convertExpr (Bool val _) = pure $ Node [Tag "literal", Tag "imaginary"] [DataBool val]
convertExpr (None _) = pure $ Node [Tag "none", Tag "null"] []
convertExpr (Ellipsis _) = pure $ Node [Tag "ellipsis", Tag "..."] []
convertExpr (ByteStrings strs _) = pure $ Node [Tag "string", Tag "bytes"] $ DataBytes . BS.C8.pack <$> strs
convertExpr (Strings strs _) = pure $ Node [Tag "string"] $ DataStr . T.pack <$> strs
convertExpr (UnicodeStrings strs _) = pure $ Node [Tag "string", Tag "unicode"] $ DataStr . T.pack <$> strs
convertExpr (Call f args _) = Node [Tag "call"] <$> ((:) <$> convertExpr f <*> mapM convertArgument args)
convertExpr (Subscript l expr _) = Node [Tag "subscript"] <$> ((+:) <$> convertExpr l <*> convertExpr expr)
convertExpr (SlicedExpr l s _) = Node [Tag "subscript", Tag "slice"] <$> ((:) <$> convertExpr l <*> mapM convertSlice s)
convertExpr (CondExpr t c f _) = Node [Tag "conditional", Tag "if", Tag "?:", Tag "ternary"] <$> ((+:) .% flip (:) <$> convertExpr c <*> convertExpr t <*> convertExpr f)
convertExpr (BinaryOp op left right _) = Node [Tag "operator", Tag "binary"] <$> ((+:) .% flip (:) <$> convertOp op <*> convertExpr left <*> convertExpr right)
convertExpr (UnaryOp op arg _) = Node [Tag "operator", Tag "unary"] <$> ((+:) <$> convertOp op <*> convertExpr arg)
convertExpr (Dot expr attr _) = Node [Tag "dot", Tag ".", Tag "element", Tag "access"] <$> ((+:) <$> convertExpr expr <*> convertIdent attr)
convertExpr (Lambda args body _) = Node [Tag "lambda", Tag "function"] <$> ((:) <$> convertExpr body <*> ((:[]) . Node [Tag "parameters"] <$> mapM convertParameter args))
convertExpr (Tuple exprs _) = Node [Tag "tuple"] <$> mapM convertExpr exprs
convertExpr (Yield arg _) = Node [Tag "tuple"] <$> mapM convertYieldArg (possibly arg)
convertExpr (Generator comp _) = Node [Tag "comprehension", Tag "generator"] <$> convertComprehension comp
convertExpr (ListComp comp _) = Node [Tag "comprehension", Tag "list"] <$> convertComprehension comp
convertExpr (List exprs _) = Node [Tag "list"] <$> mapM convertExpr exprs
convertExpr (Dictionary mapping _) = Node [Tag "dictionary", Tag "map"] <$> mapM convertDictMappingPair mapping
convertExpr (DictComp comp _) = Node [Tag "comprehension", Tag "dictionary", Tag "map"] <$> convertComprehension comp
convertExpr (Set exprs _) = Node [Tag "set"] <$> mapM convertExpr exprs
convertExpr (SetComp comp _) = Node [Tag "comprehension", Tag "set"] <$> convertComprehension comp
convertExpr (Starred expr _) = Node [Tag "starred"] . (:[]) <$> convertExpr expr
convertExpr (Paren expr _) = Node [Tag "paren"] . (:[]) <$> convertExpr expr
convertExpr (StringConversion expr _) = Node [Tag "stringconversion"] . (:[]) <$> convertExpr expr
convertExpr _ = throwError $ ParseError "Invalid Python 3 expression"

convertStatement :: Statement () -> Erring Node
convertStatement (Import items _) = Node [Tag "import"] <$> mapM convertImportItem items
convertStatement (FromImport m items _) = Node [Tag "import", Tag "from"] <$> ((+:) <$> convertImportRelative m <*> convertFromItems items)
convertStatement (While cond body els _) = Node [Tag "while", Tag "loop"]
                                         <$> ((:)
                                             <$> convertExpr cond
                                             <*> ((++)
                                                 <$> ((:[]) . Node [Tag "body"] <$> mapM convertStatement body)
                                                 <*> ((:[]) . Node [Tag "else"] <$> mapM convertStatement els)))
convertStatement (For targets generator body els _) = Node [Tag "for", Tag "loop", Tag "foreach"]
                                                    <$> ((:)
                                                        <$> convertExpr generator
                                                        <*> ((+++)
                                                            <$> ((:[]) . Node [Tag "targets"] <$> mapM convertExpr targets)
                                                            <*> ((:[]) . Node [Tag "body"] <$> mapM convertStatement body)
                                                            <*> ((:[]) . Node [Tag "else"] <$> mapM convertStatement els)))
convertStatement (Fun name args _ body _) = Node [Tag "def", Tag "define", Tag "function"]
                                          <$> ((:)
                                              <$> convertIdent name
                                              <*> ((++)
                                                  <$> ((:[]) . Node [Tag "parameters"] <$> mapM convertParameter args)
                                                  <*> ((:[]) . Node [Tag "body"] <$> mapM convertStatement body)))
convertStatement (Class name args body _) = Node [Tag "class"]
                                          <$> ((:)
                                              <$> convertIdent name
                                              <*> ((++)
                                                  <$> ((:[]) . Node [Tag "parameters"] <$> mapM convertArgument args)
                                                  <*> ((:[]) . Node [Tag "body"] <$> mapM convertStatement body)))
convertStatement (Conditional guards els _) = Node [Tag "conditional", Tag "if", Tag "switch"]
                                            <$> ((++)
                                                <$> mapM (\(c, b) -> Node [Tag "case"] <$> ((:) <$> convertExpr c <*> ((:[]) . Node [Tag "body"] <$> mapM convertStatement b))) guards
                                                <*> ((:[]) . Node [Tag "else"] <$> mapM convertStatement els))
convertStatement (Assign to expr _) = Node [Tag "assign", Tag "="] <$> ((:) <$> convertExpr expr <*> mapM convertExpr to)
convertStatement (AugmentedAssign to op expr _) = Node [Tag "augmented", Tag "assign", Tag "="] <$> ((+:) .% flip (:) <$> convertExpr expr <*> convertExpr to <*> convertAssignOp op)
convertStatement (Decorated decs def _) = Node [Tag "decorator", Tag "decorated", Tag "annotation"] <$> ((:) <$> convertStatement def <*> mapM convertDecorator decs)
convertStatement (Return expr _) = Node [Tag "return"] <$> mapM convertExpr (possibly expr)
convertStatement (Try body excepts els finally _) = Node [Tag "try"]
                                                  <$> ((++++)
                                                      <$> mapM convertHandler excepts
                                                      <*> ((:[]) . Node [Tag "body"] <$> mapM convertStatement body)
                                                      <*> ((:[]) . Node [Tag "else"] <$> mapM convertStatement els)
                                                      <*> ((:[]) . Node [Tag "finally"] <$> mapM convertStatement finally))
convertStatement (Raise r _) = Node [Tag "raise", Tag "throw"] . (:[]) <$> convertRaiseExpr r
convertStatement (With ctx body _) = Node [Tag "with"]
                                   <$> ((++)
                                       <$> mapM (\(e, a) -> Node [Tag "withitem"] <$> ((:) <$> convertExpr e <*> mapM convertExpr (possibly a))) ctx
                                       <*> ((:[]) . Node [Tag "body"] <$> mapM convertStatement body))
convertStatement (Pass _) = pure $ Node [Tag "pass", Tag "nop"] []
convertStatement (Break _) = pure $ Node [Tag "break"] []
convertStatement (Continue _) = pure $ Node [Tag "continue"] []
convertStatement (Delete exprs _) = Node [Tag "delete"] <$> mapM convertExpr exprs
convertStatement (StmtExpr expr _) = convertExpr expr
convertStatement (Global vars _) = Node [Tag "global", Tag "declare"] <$> mapM convertIdent vars
convertStatement (NonLocal vars _) = Node [Tag "nonlocal", Tag "declare"] <$> mapM convertIdent vars
convertStatement (Assert exprs _) = Node [Tag "assert"] <$> mapM convertExpr exprs
convertStatement _ = pure $ Node [Tag "unknown"] []

convertImportItem :: ImportItem () -> Erring Node
convertImportItem (ImportItem name as _) = Node [Tag "importitem"] <$> ((:) <$> convertDottedName name <*> mapM convertIdent (possibly as))

convertImportRelative :: ImportRelative () -> Erring Node
convertImportRelative (ImportRelative dots m _) = Node [Tag "import", Tag "relative"] <$> ((:) <$> pure (MetaCount dots) <*> mapM convertDottedName (possibly m))

convertFromItems :: FromItems () -> Erring Node
convertFromItems (ImportEverything _) = pure $ Node [Tag "importitem", Tag "importeverything", Tag "from"] []
convertFromItems (FromItems items _) = Node [Tag "importitem", Tag "from"] <$> mapM convertFromItem items

convertFromItem :: FromItem () -> Erring Node
convertFromItem (FromItem name as _) = Node [Tag "importitem", Tag "from"] <$> ((:) <$> convertIdent name <*> mapM convertIdent (possibly as))

convertIdent :: Ident () -> Erring Node
convertIdent (Ident s _) = pure . MetaIdent . T.pack $ s

convertParameter :: Parameter () -> Erring Node
convertParameter (Param name ann def _) = Node [Tag "parameter"]
                                        <$> ((:) .% (++)
                                            <$> convertIdent name
                                            <*> mapM convertExpr (possibly ann)
                                            <*> mapM convertExpr (possibly def))
convertParameter (VarArgsPos name ann _) = Node [Tag "varargs", Tag "positional", Tag "parameter"]
                                         <$> ((:)
                                             <$> convertIdent name
                                             <*> mapM convertExpr (possibly ann))
convertParameter (VarArgsKeyword name ann _) = Node [Tag "varargs", Tag "keyword", Tag "parameter"]
                                             <$> ((:)
                                                 <$> convertIdent name
                                                 <*> mapM convertExpr (possibly ann))
convertParameter (EndPositional _) = pure $ Node [Tag "marker", Tag "positional", Tag "parameter"] []
convertParameter (UnPackTuple tup def _) = Node [Tag "unpacktuple", Tag "parameter"]
                                         <$> ((:)
                                             <$> convertParamTuple tup
                                             <*> mapM convertExpr (possibly def))

convertArgument :: Argument () -> Erring Node
convertArgument (ArgExpr e _) = Node [Tag "argument"] . (:[]) <$> convertExpr e
convertArgument (ArgVarArgsPos e _) = Node [Tag "argument", Tag "varargs", Tag "positional"] . (:[]) <$> convertExpr e
convertArgument (ArgVarArgsKeyword e _) = Node [Tag "argument", Tag "varargs", Tag "keyword"] . (:[]) <$> convertExpr e
convertArgument (ArgKeyword kw e _) = Node [Tag "argument", Tag "keyword"] <$> ((+:) <$> convertIdent kw <*> convertExpr e)

convertAssignOp :: AssignOp () -> Erring Node
convertAssignOp (PlusAssign _) = pure $ Node [Tag "plusassign", Tag "+="] []
convertAssignOp (MinusAssign _) = pure $ Node [Tag "minusassign", Tag "-="] []
convertAssignOp (MultAssign _) = pure $ Node [Tag "multassign", Tag "*="] []
convertAssignOp (DivAssign _) = pure $ Node [Tag "divassign", Tag "/="] []
convertAssignOp (ModAssign _) = pure $ Node [Tag "modassign", Tag "%="] []
convertAssignOp (PowAssign _) = pure $ Node [Tag "powassign", Tag "**="] []
convertAssignOp (BinAndAssign _) = pure $ Node [Tag "binaryandassign", Tag "&="] []
convertAssignOp (BinOrAssign _) = pure $ Node [Tag "binaryorassign", Tag "|="] []
convertAssignOp (BinXorAssign _) = pure $ Node [Tag "binaryxorassign", Tag "^="] []
convertAssignOp (LeftShiftAssign _) = pure $ Node [Tag "leftshiftassign", Tag "<<="] []
convertAssignOp (RightShiftAssign _) = pure $ Node [Tag "rightshiftassign", Tag ">>="] []
convertAssignOp (FloorDivAssign _) = pure $ Node [Tag "floordivassign", Tag "//="] []

convertDecorator :: Decorator () -> Erring Node
convertDecorator (Decorator name args _) = Node [Tag "decorator"] <$> ((:) <$> convertDottedName name <*> mapM convertArgument args)

convertHandler :: Handler () -> Erring Node
convertHandler (Handler clause body _) = Node [Tag "handler", Tag "catch"] <$> ((:) <$> convertExceptClause clause <*> mapM convertStatement body)

convertRaiseExpr :: RaiseExpr () -> Erring Node
convertRaiseExpr (RaiseV3 Nothing) = pure $ Node [Tag "raiseexpr"] []
convertRaiseExpr (RaiseV3 (Just (e, Nothing))) = Node [Tag "raiseexpr"] . (:[]) <$> convertExpr e
convertRaiseExpr (RaiseV3 (Just (e, Just e'))) = Node [Tag "raiseexpr", Tag "raisefrom"] <$> ((+:) <$> convertExpr e <*> convertExpr e')
convertRaiseExpr _ = throwError $ ParseError "Invalid Python 3 expression (raise)"

convertParamTuple :: ParamTuple () -> Erring Node
convertParamTuple (ParamTupleName name _) = Node [Tag "name", Tag "paramtuple"] . (:[]) <$> convertIdent name
convertParamTuple (ParamTuple tuple _) = Node [Tag "paramtuple"] <$> mapM convertParamTuple tuple

convertDottedName :: DottedName () -> Erring Node
convertDottedName x = Node [Tag "dottedname"] <$> mapM convertIdent x

convertSlice :: Slice () -> Erring Node
convertSlice (SliceProper lower upper stride _) = Node [Tag "slice"]
                                                <$> ((+++)
                                                    <$> mapM convertExpr (possibly lower)
                                                    <*> mapM convertExpr (possibly upper)
                                                    <*> mapM convertExpr (possibly . join $ stride))
convertSlice (SliceExpr e _) = Node [Tag "slice", Tag "expr"] . (:[]) <$> convertExpr e
convertSlice (SliceEllipsis _) = pure $ Node [Tag "slice", Tag "ellipsis", Tag "..."] []

convertOp :: Op () -> Erring Node
convertOp (And _) = pure $ Node [Tag "and", Tag "&&"] []
convertOp (Or _) = pure $ Node [Tag "or", Tag "||"] []
convertOp (Not _) = pure $ Node [Tag "not", Tag "!"] []
convertOp (Exponent _) = pure $ Node [Tag "exponent", Tag "**"] []
convertOp (LessThan _) = pure $ Node [Tag "lessthan", Tag "<"] []
convertOp (GreaterThan _) = pure $ Node [Tag "greaterthan", Tag ">"] []
convertOp (Equality _) = pure $ Node [Tag "equality", Tag "equals", Tag "eq", Tag "=="] []
convertOp (GreaterThanEquals _) = pure $ Node [Tag "greaterthanequals", Tag "gte", Tag ">="] []
convertOp (LessThanEquals _) = pure $ Node [Tag "lessthanequals", Tag "lte", Tag "<="] []
convertOp (NotEquals _) = pure $ Node [Tag "notequals", Tag "ne", Tag "!="] []
convertOp (In _) = pure $ Node [Tag "in"] []
convertOp (Is _) = pure $ Node [Tag "is"] []
convertOp (IsNot _) = pure $ Node [Tag "is", Tag "not"] []
convertOp (NotIn _) = pure $ Node [Tag "in", Tag "not"] []
convertOp (BinaryOr _) = pure $ Node [Tag "binaryor", Tag "|"] []
convertOp (Xor _) = pure $ Node [Tag "binaryxor", Tag "^"] []
convertOp (BinaryAnd _) = pure $ Node [Tag "binaryand", Tag "&"] []
convertOp (ShiftLeft _) = pure $ Node [Tag "shiftleft", Tag "<<"] []
convertOp (ShiftRight _) = pure $ Node [Tag "shiftright", Tag ">>"] []
convertOp (Multiply _) = pure $ Node [Tag "multiply", Tag "*"] []
convertOp (Plus _) = pure $ Node [Tag "plus", Tag "add", Tag "+"] []
convertOp (Minus _) = pure $ Node [Tag "minus", Tag "subtract", Tag "-"] []
convertOp (Divide _) = pure $ Node [Tag "divide", Tag "/"] []
convertOp (FloorDivide _) = pure $ Node [Tag "intergerdiv", Tag "divide", Tag "//"] []
convertOp (Invert _) = pure $ Node [Tag "invert", Tag "~"] []
convertOp (Modulo _) = pure $ Node [Tag "modulo", Tag "%"] []
convertOp _ = throwError $ ParseError "Invalid Python 3 operator"

convertYieldArg :: YieldArg () -> Erring Node
convertYieldArg (YieldFrom expr _) = Node [Tag "yield"] . (:[]) <$> convertExpr expr
convertYieldArg (YieldExpr expr) = Node [Tag "yield"] . (:[]) <$> convertExpr expr

convertComprehension :: Comprehension () -> Erring [Node]
convertComprehension (Comprehension cexpr cfor _) = (+:) <$> convertComprehensionExpr cexpr <*> convertCompFor cfor

convertDictMappingPair :: DictMappingPair () -> Erring Node
convertDictMappingPair (DictMappingPair e e') = Node [Tag "dictpair"] <$> ((+:) <$> convertExpr e <*> convertExpr e')

convertExceptClause :: ExceptClause () -> Erring Node
convertExceptClause (ExceptClause Nothing _) = pure $ Node [Tag "exceptclause"] []
convertExceptClause (ExceptClause (Just (e, Nothing)) _) = Node [Tag "exceptclause"] . (:[]) <$> convertExpr e
convertExceptClause (ExceptClause (Just (e, Just e')) _) = Node [Tag "exceptclause"] <$> ((+:) <$> convertExpr e <*> convertExpr e')

convertComprehensionExpr :: ComprehensionExpr () -> Erring Node
convertComprehensionExpr (ComprehensionExpr e) = convertExpr e
convertComprehensionExpr (ComprehensionDict d) = convertDictMappingPair d

convertCompFor :: CompFor () -> Erring Node
convertCompFor (CompFor fexprs iexpr fiter _) = Node [Tag "compfor"]
                                              <$> ((:) .% (++)
                                                  <$> convertExpr iexpr
                                                  <*> ((:[]) . Node [Tag "exprs"] <$> mapM convertExpr fexprs)
                                                  <*> mapM convertCompIter (possibly fiter))

convertCompIf :: CompIf () -> Erring Node
convertCompIf (CompIf expr iter _) = Node [Tag "compif"] <$> ((:) <$> convertExpr expr <*> mapM convertCompIter (possibly iter))

convertCompIter :: CompIter () -> Erring Node
convertCompIter (IterFor cfor _) = convertCompFor cfor
convertCompIter (IterIf cif _) = convertCompIf cif

parse :: Parser Node
parse f d = case parseModule (BS.C8.unpack d) f of
                Left _ -> undefined
                Right (m, _) -> convertModule . void $ m
