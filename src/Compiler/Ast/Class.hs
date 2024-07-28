module Compiler.Ast.Class where

import Compiler.Ast.Shared
import Compiler.Ast.Method
import Data.HashMap.Strict as H
import Data.Maybe
import Data.List as L


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

instance TypeUtils Class where
  getTypes package types imports _parent c = do
    let parents = (maybeToList $ superClass c) ++ interfaces c
    let parentTypes = Prelude.filter (/= Nothing) $ Prelude.map (\path -> types H.!? path) $ L.concat $ Prelude.map (\(SuperClass (Path path) _) -> if length path == 1 then Prelude.map (++ path) imports else [path]) parents
    let justParentTypes = Prelude.map fromJust parentTypes
    if length parentTypes /= length parents then
      Left $ "Parent types not found for class " Prelude.++ (className c)
    else
      Right $ Prelude.foldl (\acc t -> t:acc) [(typeStart c)] $ L.concat justParentTypes
    where
      typeStart c = if (length $ (classTypeParams c)) > 0 then
                GenericType (ClassType (Path (package Prelude.++ [className c]))) (classTypeParams c)
              else
                ClassType (Path (package Prelude.++ [className c]))

