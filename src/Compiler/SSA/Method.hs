module Compiler.SSA.Method where

import Compiler.Ast.Method as Ast
import Compiler.Ast.Shared as AstShared
import Compiler.SSA.Statement
import Data.Maybe


data Method = Method
  { methodVisibility :: AstShared.Visibility
  , isStatic :: Bool
  , isAbstract :: Bool
  , isConst :: Bool
  , methodName :: String
  , methodTypeParams :: [AstShared.TypeParam]
  , params :: [Ast.Param]
  , returnType :: AstShared.Type
  , body :: Compiler.SSA.Method.MethodBody
  } deriving (Show, Eq)

data MethodBody = Prototype | MethodBody [Compiler.SSA.Statement.Statement] | Native String | Redirect String
  deriving (Show, Eq)


convertMethod :: Ast.Method -> Compiler.SSA.Method.Method
convertMethod Ast.Method{ Ast.methodVisibility=methodVisibility, Ast.isStatic=isStatic, Ast.isAbstract=isAbstract, Ast.isConst=isConst, Ast.methodName=methodName, Ast.methodTypeParams=methodTypeParams, Ast.params=params, Ast.returnType=returnType, Ast.body=body} = Compiler.SSA.Method.Method
  { Compiler.SSA.Method.methodVisibility=methodVisibility
  , Compiler.SSA.Method.isStatic=isStatic
  , Compiler.SSA.Method.isAbstract=isAbstract
  , Compiler.SSA.Method.isConst=isConst
  , Compiler.SSA.Method.methodName=methodName
  , Compiler.SSA.Method.methodTypeParams=methodTypeParams
  , Compiler.SSA.Method.params=params
  , Compiler.SSA.Method.returnType=returnType
  , Compiler.SSA.Method.body=convertMethodBody body
  }

convertMethodBody :: Ast.MethodBody -> Compiler.SSA.Method.MethodBody
convertMethodBody Ast.Prototype = Compiler.SSA.Method.Prototype
convertMethodBody (Ast.MethodBody statements) = Compiler.SSA.Method.MethodBody $ convertStatements statements
convertMethodBody (Ast.Native s) = Compiler.SSA.Method.Native s
convertMethodBody (Ast.Redirect s) = Compiler.SSA.Method.Redirect s

