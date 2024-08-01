module Compiler.Backend.ClassFile where

import Compiler.Backend.Bytecode


data PoolEntry = U8Entry Int8
  | I8Entry Int8
  | U16Entry Int16
  | I16Entry Int16
  | U32Entry Int32
  | I32Entry Int32
  | U64Entry Int64
  | I64Entry Int64
  | F32Entry Float
  | F64Entry Double
  | CharEntry Char
  | StringEntry String
  | ClassInfoEntry ClassInfo
  | Method [Bytecode]
  | TypeInfoEntry TypeInfo
  | RedirectEntry Integer
  deriving (Show, Eq)

data ClassInfo = ClassInfo
  { className :: Integer
  } deriving (Show, Eq)

data Method = NativeMethod Integer
  | BytecodeMethod [Bytecode]
  | ForeignMethod Integer
  deriving (Show, Eq)

data TypeInfo = UnitType
  | U8Type
  | I8Type
  | U16Type
  | I16Type
  | U32Type
  | I32Type
  | U64Type
  | I64Type
  | F32Type
  | F64Type
  | CharType
  | StringType
  | ArrayType TypeInfo
  | ObjectType Integer
  | MethodType [TypeInfo] TypeInfo
  deriving (Show, Eq)

data FieldInfo = FieldInfo
  { fieldName :: Integer
  , flags :: Int8
  , fieldType :: Integer
  } deriving (Show, Eq)

data MethodInfo = MethodInfo
  { methodName :: Integer
  , flags :: Int8
  , methodType :: Integer
  , location :: Integer
  } deriving (Show, Eq)

data InterfaceInfo = InterfaceInfo
  { interfaceName :: Integer
  , vtable :: [Integer]
  } deriving (Show, Eq)

data ClassFile = ClassFile
  { thisInfo :: Integer
  , superInfo :: Integer
  , classFlags :: Int8
  , constantPool :: [PoolEntry]
  , fields :: [FieldInfo]
  , methods :: [MethodInfo]
  , strings :: [Integer]
  } deriving (Show, Eq)
