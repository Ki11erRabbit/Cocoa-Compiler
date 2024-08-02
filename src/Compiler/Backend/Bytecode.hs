module Compiler.Backend.Bytecode (Bytecode, OpType) where

data OpType = I8 | I16 | I32 | I64 | F32 | F64 | U8 | U16 | U32 | U64 | Char Int | Reference
  deriving (Show, Eq)


data Bytecode = Pop
  | PushNull
  | LoadConstant Integer
  | Dup
  | Swap
  | StoreLocal Int -- largest is 255
  | LoadLocal Int -- largest is 255
  | Add
  | Subtract
  | Multiply
  | Divide
  | Modulo
  | Negate
  | And
  | Or
  | Xor
  | Not
  | ShiftLeft
  | ShiftRight
  | Equal
  | Greater
  | Less
  | Convert OpType
  | BinaryConvert OpType
  | GoTo Integer
  | If Integer
  | IfNot Integer
  | IfGreater Integer
  | IfGreaterEqual Integer
  | IfLess Integer
  | IfLessEqual Integer
  | IfNull Integer
  | IfNotNull Integer
  | InvokeVirtual Integer
  | InvokeVirtualTail Integer
  | InvokeStatic Integer Integer
  | InvokeStaticTail Integer Integer
  | InvokeInterface Integer Integer
  | InvokeInterfaceTail Integer Integer
  | InvokeInterfaceStatic Integer Integer Integer
  | InvokeInterfaceStaticTail Integer Integer Integer
  | Return
  | ReturnUnit
  | New Integer
  | SetField Integer
  | GetField Integer
  | StoreStatic Integer
  | LoadStatic Integer
  | InstanceOf Integer
  | GetParent
  | NewArray OpType
  | ArrayGet OpType
  | ArraySet OpType
  | NewString Integer
  | Breakpoint
  | Nop
  deriving (Show, Eq)
