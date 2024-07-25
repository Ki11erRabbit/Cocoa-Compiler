module Compiler.Ast.Statement where

import Compiler.Ast.Expr
import Compiler.Ast.Shared

data Statement = WhileStmt WhileStatement | ForStmt ForStatement | IfStmt IfExpr | ReturnStmt ReturnStatement | LetStmt LetStatement | ExprStmt Expr | BreakStmt BreakStatement | ContinueStmt ContinueStatement | Hanging Expr
  deriving (Show, Eq)

data WhileStatement = WhileStatement Expr [Statement]
  deriving (Show, Eq)

data ForStatement = ForStatement String Expr [Statement]
  deriving (Show, Eq)

data ReturnStatement = ReturnExpr Expr | ReturnUnit
  deriving (Show, Eq)

data LetStatement = LetStatement String (Maybe Type) Expr
  deriving (Show, Eq)

data BreakStatement = BreakStatement
  deriving (Show, Eq)

data ContinueStatement = ContinueStatement
  deriving (Show, Eq)

data IfExpr = IfExpr Expr [Statement] (Maybe (Either [Statement] IfExpr))
  deriving (Show, Eq)
