{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Expr.Ops where


import Compiler.Ast.Expr
import Compiler.Parser.Shared
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char


table = [ [prefix "!" (UnaryOp NotOp), prefix "-" (UnaryOp Neg)]
        , [binary "*" (BinaryOp Mul), binary "/" (BinaryOp Div), binary "%" (BinaryOp Mod)]
        , [binary "+" (BinaryOp Add), binary "-" (BinaryOp Sub)]
        , [binary "<<" (BinaryOp LShift), binary ">>" (BinaryOp RShift)]
        , [binary "<" (BinaryOp Lt), binary ">" (BinaryOp Gt), binary "<=" (BinaryOp Le), binary ">=" (BinaryOp Ge)]
        , [binary "==" (BinaryOp Eq), binary "!=" (BinaryOp Neq)]
        , [binary "&" (BinaryOp BitAnd)]
        , [binary "^" (BinaryOp BitXor)]
        , [binary "|" (BinaryOp BitOr)]
        , [binary "&&" (BinaryOp LogicalAnd)]
        , [binary "||" (BinaryOp LogicalOr)]
        , [binary ".." (BinaryOp ExclusiveRangeOp), binary "..=" (BinaryOp InclusiveRangeOp)]
        ]

binary name f = InfixL (do _ <- string name; _ <- skipParser; return f)
prefix name f = Prefix (do _ <- string name; _ <- skipParser; return f)

parseOp :: Parser Expr -> Parser Expr
parseOp term = do
  makeExprParser (term <|> (parseOp term) <?> "term") table <?> "expression"


