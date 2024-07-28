module Compiler.Ast.Shared where

import Data.HashMap.Strict as H

data Path = Path [String]
  deriving (Show, Eq)

data Visibility = PublicVis | ProtectedVis | PrivateVis
  deriving (Show, Eq)

data Type = Primitive PrimitiveType | Array Type | ClassType Path | Arguments Type [Type] | GenericType Type [TypeParam]
  deriving (Show, Eq)

data PrimitiveType = UnitPrimType | U8PrimType | U16PrimType | U32PrimType | U64PrimType | I8PrimType | I16PrimType | I32PrimType | I64PrimType | F32PrimType | F64PrimType | BoolPrimType | CharPrimType
  deriving (Show, Eq)

--data ClosureBody = ClosureBody [Statement]
--  deriving (Show)


data TypeParam = Generic String [Type] | Concrete Type
  deriving (Show, Eq)

class TypeUtils a where
  getTypes :: [String] -> H.HashMap [String] [Type] -> [[String]] -> Maybe Type -> a -> Either String [Type]
