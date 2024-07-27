module Compiler.Ast.Class where

import Compiler.Ast.Shared
import Compiler.Ast.Method


data SuperClass = SuperClass Path [TypeParam]
  deriving (Show, Eq)

data Class = Class
  { visibility :: Visibility
  , className :: String
  , classType :: ClassType
  , classTypeParams :: [TypeParam]
  , superClass :: Maybe SuperClass
  , interfaces :: [SuperClass]
  , members :: [Member]
  } deriving (Show, Eq)

data ClassType = RegularType | InterfaceType | AbstractType
  deriving (Show, Eq)


data Member = FieldMember Field | MethodMember Method | ClassMember Class
  deriving (Show, Eq)


data Field = Field
  { fieldVisibility :: Visibility
  , fieldName :: String
  , fieldType :: Type
  } deriving (Show, Eq)
