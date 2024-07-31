module Compiler.SSA.Class where

import Compiler.SSA.Shared
import Compiler.SSA.Method
import Data.HashMap.Strict as H
import Data.Maybe
import Data.List as L
import Debug.Trace (trace)


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
