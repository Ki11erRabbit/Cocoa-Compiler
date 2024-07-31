module Compiler.SSA.Statement where

import Compiler.Ast.Shared as AstShared
import Compiler.Ast.Statement as Ast
import Data.HashMap.Strict as H
import Control.Monad.State
import Data.Maybe

type MyState = H.HashMap String Int

getVarVersion :: String -> State MyState Int
getVarVersion var = do
  state <- get
  case H.lookup var state of
    Just version -> return version
    Nothing -> do
      put $ H.insert var 0 state
      return 0

lookupVarVersion :: String -> State MyState Int
lookupVarVersion var = do
  state <- get
  case H.lookup var state of
    Just version -> return version
    Nothing -> error $ "Variable " ++ var ++ " not found in state"


convertStatements :: [Ast.Statement] -> [Compiler.SSA.Statement.Statement]
convertStatements stmts = evalState (convertStatements' stmts) H.empty

convertStatements' :: [Ast.Statement] -> State MyState [Compiler.SSA.Statement.Statement]
convertStatements' [] = return []
convertStatements' stmts = Prelude.mapM convertStatement stmts

convertStatement :: Ast.Statement -> State MyState Compiler.SSA.Statement.Statement
convertStatement (Ast.WhileStmt (Ast.WhileStatement expr stmts)) = do
  convertedStmts <- convertStatements' stmts
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.WhileStmt (Compiler.SSA.Statement.WhileStatement expr' convertedStmts)
convertStatement (Ast.ForStmt (Ast.ForStatement letVar expr stmts)) = do
  convertedStmts <- convertStatements' stmts
  expr' <- convertExpr expr
  letVar' <- convertLetVar letVar
  return $ Compiler.SSA.Statement.ForStmt (Compiler.SSA.Statement.ForStatement letVar' expr' convertedStmts)
convertStatement (Ast.IfStmt ifExpr) = do
  ifExpr' <- convertIfExpr ifExpr
  return $ Compiler.SSA.Statement.IfStmt ifExpr'
convertStatement (Ast.ReturnStmt (Ast.ReturnExpr expr)) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.ReturnStmt (Compiler.SSA.Statement.ReturnExpr expr')
convertStatement (Ast.ReturnStmt Ast.ReturnUnit) = return $ Compiler.SSA.Statement.ReturnStmt Compiler.SSA.Statement.ReturnUnit
convertStatement (Ast.LetStmt (Ast.LetStatement letVar expr)) = do
  expr' <- convertExpr expr
  letVar' <- convertLetVar letVar
  return $ Compiler.SSA.Statement.LetStmt (Compiler.SSA.Statement.LetStatement letVar' expr')
convertStatement (Ast.ExprStmt expr) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.ExprStmt expr'
convertStatement (Ast.BreakStmt _) = return $ Compiler.SSA.Statement.BreakStmt Compiler.SSA.Statement.BreakStatement
convertStatement (Ast.ContinueStmt _) = return $ Compiler.SSA.Statement.ContinueStmt Compiler.SSA.Statement.ContinueStatement
convertStatement (Ast.Hanging expr) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.Hanging expr'
convertStatement (Ast.AssignmentStmt lhs rhs) = do
  lhs' <- convertExprLhs lhs
  rhs' <- convertExpr rhs
  return $ Compiler.SSA.Statement.AssignmentStmt lhs' rhs'
convertStatement _ = error "Statement not implemented"


convertExpr :: Ast.Expr -> State MyState Compiler.SSA.Statement.Expr
convertExpr (Ast.BinaryOp op lhs rhs) = do
  lhs' <- convertExpr lhs
  rhs' <- convertExpr rhs
  let op' = convertBinaryOp op
  return $ Compiler.SSA.Statement.BinaryOp op' lhs' rhs'
convertExpr (Ast.UnaryOp op expr) = do
  expr' <- convertExpr expr
  let op' = convertUnaryOp op
  return $ Compiler.SSA.Statement.UnaryOp op' expr'
convertExpr (Ast.Call expr args) = do
  expr' <- convertExpr expr
  args' <- Prelude.mapM convertExpr args
  return $ Compiler.SSA.Statement.Call expr' args'
convertExpr (Ast.NewExpr typ args) = do
  args' <- Prelude.mapM convertExpr args
  return $ Compiler.SSA.Statement.NewExpr typ args'
convertExpr (Ast.Var var) = do
  var' <- getVarVersion var
  return $ Compiler.SSA.Statement.Var (var Prelude.++ "_v" Prelude.++ show var')
convertExpr (Ast.FieldAccess expr field) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.FieldAccess expr' field
convertExpr (Ast.ArrayAccess expr index) = do
  expr' <- convertExpr expr
  index' <- convertExpr index
  return $ Compiler.SSA.Statement.ArrayAccess expr' index'
convertExpr (Ast.ArrayLiteral exprs) = do
  exprs' <- Prelude.mapM convertExpr exprs
  return $ Compiler.SSA.Statement.ArrayLiteral exprs'
convertExpr (Ast.Literal lit) = return $ Compiler.SSA.Statement.Literal $ convertLiteral lit
convertExpr (Ast.ThisExpr) = return Compiler.SSA.Statement.ThisExpr
convertExpr (Ast.SuperExpr) = return Compiler.SSA.Statement.SuperExpr
convertExpr (Ast.NullExpr) = return Compiler.SSA.Statement.NullExpr
convertExpr (Ast.Cast typ expr) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.Cast typ expr'
convertExpr (Ast.InstanceOf expr typ) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.InstanceOf expr' typ
convertExpr (Ast.Paren expr) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.Paren expr'
convertExpr (Ast.IfExprExpr ifExpr) = do
  ifExpr' <- convertIfExpr ifExpr
  return $ Compiler.SSA.Statement.IfExprExpr ifExpr'
convertExpr (Ast.BlockExpr stmts) = do
  stmts' <- convertStatements' stmts
  return $ Compiler.SSA.Statement.BlockExpr stmts'
convertExpr _ = error "Expr not implemented"

convertExprLhs :: Ast.Expr -> State MyState Compiler.SSA.Statement.Expr
convertExprLhs (Ast.Var var) = do
  var' <- getVarVersion var
  return $ Compiler.SSA.Statement.Var (var Prelude.++ "_v" Prelude.++ show var')
convertExprLhs (Ast.FieldAccess expr field) = do
  expr' <- convertExpr expr
  return $ Compiler.SSA.Statement.FieldAccess expr' field
convertExprLhs (Ast.ArrayAccess expr index) = do
  expr' <- convertExpr expr
  index' <- convertExpr index
  return $ Compiler.SSA.Statement.ArrayAccess expr' index'
convertExprLhs _ = error "Expr not implemented"

convertBinaryOp :: Ast.BinaryOp -> Compiler.SSA.Statement.BinaryOp
convertBinaryOp Ast.Add = Compiler.SSA.Statement.Add
convertBinaryOp Ast.Sub = Compiler.SSA.Statement.Sub
convertBinaryOp Ast.Mul = Compiler.SSA.Statement.Mul
convertBinaryOp Ast.Div = Compiler.SSA.Statement.Div
convertBinaryOp Ast.Mod = Compiler.SSA.Statement.Mod
convertBinaryOp Ast.Eq = Compiler.SSA.Statement.Eq
convertBinaryOp Ast.Neq = Compiler.SSA.Statement.Neq
convertBinaryOp Ast.Lt = Compiler.SSA.Statement.Lt
convertBinaryOp Ast.Gt = Compiler.SSA.Statement.Gt
convertBinaryOp Ast.Le = Compiler.SSA.Statement.Le
convertBinaryOp Ast.Ge = Compiler.SSA.Statement.Ge
convertBinaryOp Ast.BitAnd = Compiler.SSA.Statement.BitAnd
convertBinaryOp Ast.BitOr = Compiler.SSA.Statement.BitOr
convertBinaryOp Ast.BitXor = Compiler.SSA.Statement.BitXor
convertBinaryOp Ast.LShift = Compiler.SSA.Statement.LShift
convertBinaryOp Ast.RShift = Compiler.SSA.Statement.RShift
convertBinaryOp Ast.ExclusiveRangeOp = Compiler.SSA.Statement.ExclusiveRangeOp
convertBinaryOp Ast.InclusiveRangeOp = Compiler.SSA.Statement.InclusiveRangeOp
convertBinaryOp Ast.LogicalAnd = Compiler.SSA.Statement.LogicalAnd
convertBinaryOp Ast.LogicalOr = Compiler.SSA.Statement.LogicalOr

convertUnaryOp :: Ast.UnaryOp -> Compiler.SSA.Statement.UnaryOp
convertUnaryOp Ast.Neg = Compiler.SSA.Statement.Neg
convertUnaryOp Ast.NotOp = Compiler.SSA.Statement.NotOp

convertLiteral :: Ast.Literal -> Compiler.SSA.Statement.Literal
convertLiteral (Ast.IntLit lit) = Compiler.SSA.Statement.IntLit lit
convertLiteral (Ast.FloatLit lit) = Compiler.SSA.Statement.FloatLit lit
convertLiteral (Ast.BoolLit lit) = Compiler.SSA.Statement.BoolLit lit
convertLiteral (Ast.CharLit lit) = Compiler.SSA.Statement.CharLit lit
convertLiteral (Ast.StringLit lit) = Compiler.SSA.Statement.StringLit lit

convertIfExpr :: Ast.IfExpr -> State MyState Compiler.SSA.Statement.IfExpr
convertIfExpr (Ast.IfExpr expr stmts elseExpr) = do
  expr' <- convertExpr expr
  stmts' <- convertStatements' stmts
  elseExpr' <- case elseExpr of
    Just (Left stmts) -> do
      stmts' <- convertStatements' stmts
      return $ Just $ Left stmts'
    Just (Right ifExpr) -> do
      ifExpr' <- convertIfExpr ifExpr
      return $ Just $ Right ifExpr'
    Nothing -> return Nothing
  return $ Compiler.SSA.Statement.IfExpr expr' stmts' elseExpr'

convertLetVar :: Ast.LetVar -> State MyState Compiler.SSA.Statement.LetVar
convertLetVar (Ast.LetVar var typ) = do
  var' <- getVarVersion var
  return $ Compiler.SSA.Statement.LetVar (var Prelude.++ "_v" Prelude.++ show var') $ fromJust typ


data Statement = WhileStmt Compiler.SSA.Statement.WhileStatement
  | ForStmt Compiler.SSA.Statement.ForStatement
  | IfStmt Compiler.SSA.Statement.IfExpr
  | ReturnStmt Compiler.SSA.Statement.ReturnStatement
  | LetStmt Compiler.SSA.Statement.LetStatement
  | ExprStmt Compiler.SSA.Statement.Expr
  | BreakStmt Compiler.SSA.Statement.BreakStatement
  | ContinueStmt Compiler.SSA.Statement.ContinueStatement
  | Hanging Compiler.SSA.Statement.Expr
  | AssignmentStmt Compiler.SSA.Statement.Expr Compiler.SSA.Statement.Expr
  deriving (Show, Eq)

data WhileStatement = WhileStatement Compiler.SSA.Statement.Expr [Compiler.SSA.Statement.Statement]
  deriving (Show, Eq)

data ForStatement = ForStatement Compiler.SSA.Statement.LetVar Compiler.SSA.Statement.Expr [Compiler.SSA.Statement.Statement]
  deriving (Show, Eq)

data ReturnStatement = ReturnExpr Compiler.SSA.Statement.Expr | ReturnUnit
  deriving (Show, Eq)

data LetStatement = LetStatement Compiler.SSA.Statement.LetVar Compiler.SSA.Statement.Expr
  deriving (Show, Eq)

data BreakStatement = BreakStatement
  deriving (Show, Eq)

data ContinueStatement = ContinueStatement
  deriving (Show, Eq)

data IfExpr = IfExpr Compiler.SSA.Statement.Expr [Compiler.SSA.Statement.Statement] (Maybe (Either [Compiler.SSA.Statement.Statement] Compiler.SSA.Statement.IfExpr))
  deriving (Show, Eq)

data LetVar = LetVar String AstShared.Type
  deriving (Show, Eq)



data Expr =
    BinaryOp Compiler.SSA.Statement.BinaryOp Compiler.SSA.Statement.Expr Compiler.SSA.Statement.Expr
  | UnaryOp Compiler.SSA.Statement.UnaryOp Compiler.SSA.Statement.Expr
  | Call Compiler.SSA.Statement.Expr [Compiler.SSA.Statement.Expr]
  | NewExpr AstShared.Type [Compiler.SSA.Statement.Expr]
  | Var String
  | FieldAccess Compiler.SSA.Statement.Expr String
  | ArrayAccess Compiler.SSA.Statement.Expr Compiler.SSA.Statement.Expr
  | ArrayLiteral [Compiler.SSA.Statement.Expr]
  | Literal Compiler.SSA.Statement.Literal
  | ThisExpr
  | SuperExpr
  | NullExpr
  | Cast AstShared.Type Compiler.SSA.Statement.Expr
  | InstanceOf Compiler.SSA.Statement.Expr AstShared.Type
  | Paren Compiler.SSA.Statement.Expr
  | IfExprExpr Compiler.SSA.Statement.IfExpr
  | BlockExpr [Compiler.SSA.Statement.Statement]
-- | Closure [ClosureParam] ClosureBody
  deriving (Show, Eq)

data BinaryOp = Add | Sub | Mul | Div | Mod | Eq | Neq | Lt | Gt | Le | Ge | BitAnd | BitOr | BitXor | LShift | RShift | ExclusiveRangeOp | InclusiveRangeOp | LogicalAnd | LogicalOr
  deriving (Show, Eq)

data UnaryOp = Neg | NotOp
  deriving (Show, Eq)

data Literal = IntLit String | FloatLit String | BoolLit Bool | CharLit Char | StringLit String
  deriving (Show, Eq)

data ClosureParam = ClosureTyped String AstShared.Type | ClosureUnTyped String
  deriving (Show, Eq)
