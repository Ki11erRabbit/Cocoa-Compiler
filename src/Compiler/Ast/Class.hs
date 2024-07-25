module Compiler.Ast.Class where

import Compiler.Ast.Shared
import Compiler.Ast.Method


data SuperClass = SuperClass Path [TypeParam]
  deriving (Show)

data Class = Class
  { visibility :: Visibility
  , className :: String
  , classType :: ClassType
  , classTypeParams :: [TypeParam]
  , superClass :: Maybe SuperClass
  , interfaces :: [SuperClass]
  , members :: [Member]
  } deriving (Show)

data ClassType = RegularType | InterfaceType | AbstractType
  deriving (Show)


data Member = FieldMember Field | MethodMember Method
  deriving (Show)


data Field = Field
  { fieldVisibility :: Visibility
  , fieldName :: String
  , fieldType :: Type
  } deriving (Show)
