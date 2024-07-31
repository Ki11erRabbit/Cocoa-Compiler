module Compiler.SSA.Class where

import Compiler.Ast.Shared as AstShared
import Compiler.Ast.Class as AstClass

import Compiler.SSA.Method
import Data.HashMap.Strict as H
import Data.Maybe
import Data.List as L
import Debug.Trace (trace)


data Class = Class
  { visibility :: AstShared.Visibility
  , className :: String
  , classType :: AstClass.ClassType
  , classTypeParams :: [AstShared.TypeParam]
  , superClass :: Maybe AstClass.SuperClass
  , interfaces :: [AstClass.SuperClass]
  , members :: [Compiler.SSA.Class.Member]
  } deriving (Show, Eq)

data Member = FieldMember AstClass.Field | MethodMember Compiler.SSA.Method.Method | ClassMember Compiler.SSA.Class.Class
  deriving (Show, Eq)

convertClass :: AstClass.Class -> Compiler.SSA.Class.Class
convertClass AstClass.Class{ AstClass.visibility=visibility, AstClass.className=className, AstClass.classType=classType, AstClass.classTypeParams=classTypeParams, AstClass.superClass=superClass, AstClass.interfaces=interfaces, AstClass.members=members} = Compiler.SSA.Class.Class
  { Compiler.SSA.Class.visibility=visibility
  , Compiler.SSA.Class.className=className
  , Compiler.SSA.Class.classType=classType
  , Compiler.SSA.Class.classTypeParams=classTypeParams
  , Compiler.SSA.Class.superClass=superClass
  , Compiler.SSA.Class.interfaces=interfaces
  , Compiler.SSA.Class.members=convertMembers members
  }


convertMembers :: [AstClass.Member] -> [Compiler.SSA.Class.Member]
convertMembers [] = []
convertMembers (m:ms) = (case m of
  AstClass.FieldMember f -> AstClass.FieldMember f
  AstClass.MethodMember m -> AstClass.MethodMember (convertMethod m)
  AstClass.ClassMember c -> AstClass.ClassMember (convertClass c)
  ):(convertMembers ms)
  
  
  
