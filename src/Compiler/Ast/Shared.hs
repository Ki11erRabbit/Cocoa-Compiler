module Compiler.Ast.Shared where

import Data.HashMap.Strict as H
import Data.Hashable

data Path = Path [String]
  deriving (Show, Eq)

instance Hashable Path where
  hashWithSalt salt (Path path) = hashWithSalt salt path


data Visibility = PublicVis | ProtectedVis | PrivateVis
  deriving (Show, Eq, Enum)

instance Hashable Visibility where
  hashWithSalt = hashUsing fromEnum

data Type = Primitive PrimitiveType | Array Type | ClassType Path | Arguments Type [Type] | GenericType Type [TypeParam] | MethodType [TypeParam] [Type] Type
  deriving (Show, Eq)

instance Hashable Type where
  hashWithSalt salt (Primitive p) = hashWithSalt salt p
  hashWithSalt salt (Array t) = hashWithSalt salt t
  hashWithSalt salt (ClassType p) = hashWithSalt salt p
  hashWithSalt salt (Arguments t ts) = hashWithSalt salt (t, ts)
  hashWithSalt salt (GenericType t ts) = hashWithSalt salt (t, ts)
  hashWithSalt salt (MethodType ts ts' t) = hashWithSalt salt (ts, ts', t)

data PrimitiveType = UnitPrimType | U8PrimType | U16PrimType | U32PrimType | U64PrimType | I8PrimType | I16PrimType | I32PrimType | I64PrimType | F32PrimType | F64PrimType | BoolPrimType | CharPrimType
  deriving (Show, Eq, Enum)

instance Hashable PrimitiveType where
  hashWithSalt = hashUsing fromEnum

--data ClosureBody = ClosureBody [Statement]
--  deriving (Show)


data TypeParam = Generic String [Type] | Concrete Type
  deriving (Show, Eq)

instance Hashable TypeParam where
  hashWithSalt salt (Generic s ts) = hashWithSalt salt (s, ts)
  hashWithSalt salt (Concrete t) = hashWithSalt salt t

class TypeUtils a where
  getTypes :: [String] -> H.HashMap [String] [Type] -> [[String]] -> Maybe Type -> a -> Either String [Type]
