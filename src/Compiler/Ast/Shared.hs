module Compiler.Ast.Shared where


data Path = Path [String]
  deriving (Show, Eq)

data Visibility = PublicVis | ProtectedVis | PrivateVis
  deriving (Show, Eq)

data Type = Primitive PrimitiveType | Array Type | ClassType Path | Arguments Type [Type]
  deriving (Show, Eq)

data PrimitiveType = UnitPrimType | U8PrimType | U16PrimType | U32PrimType | U64PrimType | I8PrimType | I16PrimType | I32PrimType | I64PrimType | F32PrimType | F64PrimType | BoolPrimType | CharPrimType
  deriving (Show, Eq)

--data ClosureBody = ClosureBody [Statement]
--  deriving (Show)


data TypeParam = TypeParam
  { typeName :: String
  , bounds :: [Type]
  } deriving (Show, Eq)
