module Ast.Statement  where

import Ast.Shared

data Statement = ExpressionStmt (Spanned Expression)
               | HangingStmt (Spanned Expression)
               deriving (Show, Eq)


data Expression = SimpleExpr SimpleExpression
               | ReturnExpr (Maybe (Spanned Expression))
               deriving (Show, Eq)


data SimpleExpression = BinaryExpr BinaryOp (Spanned Expression) (Spanned Expression)
                      | PrefixExpr PrefixOp (Spanned Expression)
                      | PostfixExpr PostfixOp (Spanned Expression)
                      | LiteralExpr Literal
                      deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | BitAnd | BitOr | BitXor | LShift | RShift | ExclusiveRangeOp | InclusiveRangeOp | LogicalAnd | LogicalOr
  deriving (Show, Eq)

data PrefixOp = Neg | NotOp
  deriving (Show, Eq)

data PostfixOp = TryOp
  deriving (Show, Eq)


data Literal = U8Lit String
             | U16Lit String
             | U32Lit String
             | U64Lit String
             | I8Lit String
             | I16Lit String
             | I32Lit String
             | I64Lit String
             | IntLit String
             | F32Lit String
             | F64Lit String
             | BoolLit Bool
             | CharLit Char
             | StringLit String
             deriving (Show, Eq)

