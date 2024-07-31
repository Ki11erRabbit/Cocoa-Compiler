module Compiler.SSA.Method where

import Compiler.SSA.Shared
import Compiler.SSA.Statement
import Data.Maybe


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
  } deriving (Show, Eq)

data MethodBody = Prototype | MethodBody [Statement] | Native String | Redirect String
  deriving (Show, Eq)

data Param = Param
  { paramName :: String
  , paramType :: Type
  } deriving (Show, Eq)

instance TypeUtils Method where
  getTypes package types imports _parentClass Method{isStatic=True,methodTypeParams=typeParams,params=params,returnType=returnType} = do
    Right [(MethodType typeParams (Prelude.map paramType params) returnType)]
    
  getTypes package types imports parentClass Method{isStatic=False,methodTypeParams=typeParams,params=params,returnType=returnType} = do
    Right [(MethodType typeParams ((maybeToList parentClass) ++ (Prelude.map paramType params)) returnType)]
