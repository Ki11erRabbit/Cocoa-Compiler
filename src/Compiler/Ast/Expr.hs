module Compiler.Ast.Expr where

import Compiler.Ast.Shared

data Expr =
    BinaryOp BinaryOp Expr Expr
  | UnaryOp UnaryOp Expr
  | Call Expr [Expr]
  | NewExpr Path [Expr]
  | Var String
  | FieldAccess Expr String
  | ArrayAccess Expr Expr
  | ArrayLiteral [Expr]
  | Literal Literal
  | StaticAccess Path
  | ThisExpr
  | SuperExpr
  | NullExpr
  | Cast Type Expr
  | InstanceOf Expr Type
  | Paren Expr
--  | IfExprExpr IfExpr
-- | Closure [ClosureParam] ClosureBody
  deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod | And | Or | Eq | Neq | Lt | Gt | Le | Ge | BitAnd | BitOr | BitXor | LShift | RShift | ExclusiveRangeOp | InclusiveRangeOp | LogicalAnd | LogicalOr
  deriving (Show, Eq)

data UnaryOp = Neg | NotOp
  deriving (Show, Eq)

data Literal = IntLit String | FloatLit String | BoolLit Bool | CharLit Char | StringLit String
  deriving (Show, Eq)

data ClosureParam = ClosureTyped String Type | ClosureUnTyped String
  deriving (Show, Eq)
