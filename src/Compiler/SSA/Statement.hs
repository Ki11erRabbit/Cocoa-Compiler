module Compiler.SSA.Statement where

import Compiler.SSA.Shared

data Statement = WhileStmt WhileStatement | ForStmt ForStatement | IfStmt IfExpr | ReturnStmt ReturnStatement | LetStmt LetStatement | ExprStmt Expr | BreakStmt BreakStatement | ContinueStmt ContinueStatement | Hanging Expr | AssignmentStmt Expr Expr
  deriving (Show, Eq)

data WhileStatement = WhileStatement Expr [Statement]
  deriving (Show, Eq)

data ForStatement = ForStatement LetVar Expr [Statement]
  deriving (Show, Eq)

data ReturnStatement = ReturnExpr Expr | ReturnUnit
  deriving (Show, Eq)

data LetStatement = LetStatement LetVar Expr
  deriving (Show, Eq)

data BreakStatement = BreakStatement
  deriving (Show, Eq)

data ContinueStatement = ContinueStatement
  deriving (Show, Eq)

data IfExpr = IfExpr Expr [Statement] (Maybe (Either [Statement] IfExpr))
  deriving (Show, Eq)

data LetVar = LetVar String Type
  deriving (Show, Eq)



data Expr =
    BinaryOp BinaryOp Expr Expr
  | UnaryOp UnaryOp Expr
  | Call Expr [Expr]
  | NewExpr Type [Expr]
  | Var String
  | FieldAccess Expr String
  | ArrayAccess Expr Expr
  | ArrayLiteral [Expr]
  | Literal Literal
  | ThisExpr
  | SuperExpr
  | NullExpr
  | Cast Type Expr
  | InstanceOf Expr Type
  | Paren Expr
  | IfExprExpr IfExpr
  | BlockExpr [Statement]
-- | Closure [ClosureParam] ClosureBody
  deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | BitAnd | BitOr | BitXor | LShift | RShift | ExclusiveRangeOp | InclusiveRangeOp | LogicalAnd | LogicalOr
  deriving (Show, Eq)

data UnaryOp = Neg | NotOp
  deriving (Show, Eq)

data Literal = IntLit String | FloatLit String | BoolLit Bool | CharLit Char | StringLit String
  deriving (Show, Eq)

data ClosureParam = ClosureTyped String Type | ClosureUnTyped String
  deriving (Show, Eq)
