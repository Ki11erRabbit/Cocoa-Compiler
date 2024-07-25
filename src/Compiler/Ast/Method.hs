module Compiler.Ast.Method where

import Compiler.Ast.Shared
import Compiler.Ast.Statement

data Method = Method
  { methodVisibility :: Visibility
  , isStatic :: Bool
  , isAbstract :: Bool
  , isConst :: Bool
  , methodName :: String
  , methodTypeParams :: [TypeParam]
  , params :: [Param]
  , returnType :: Type
  , body :: MethodBody
  } deriving (Show)

data MethodBody = Prototype | MethodBody [Statement] | Native String | Redirect String
  deriving (Show)

data Param = Param
  { paramName :: String
  , paramType :: Type
  } deriving (Show, Eq)
