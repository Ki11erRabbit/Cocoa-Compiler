module Compiler.TypeChecker where

import Compiler.Ast.File
import Compiler.Ast.Class
import Compiler.Ast.Method
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Data.HashMap.Strict as H
import Data.HashSet as S
import Data.Vector as V
import Debug.Trace (trace)

emptyMapWithObject :: H.HashMap [String] [Type]
emptyMapWithObject = H.insert ["Object"] [ClassType (Path ["Object"])] H.empty

emptyMapWithoutObject :: H.HashMap [String] [Type]
emptyMapWithoutObject = H.empty


topLevelTypeCheck :: V.Vector File -> [Int] -> Either String ()
topLevelTypeCheck files order = checkFiles files order emptyMapWithObject


checkFiles :: V.Vector File -> [Int] -> H.HashMap [String] [Type] -> Either String ()
checkFiles files (next:rest) types = do
  let file = files V.! next
  newTypes <- trace "checking file" $ checkFile file types
  checkFiles files rest newTypes
checkFiles files [] types = Right ()

checkFile :: File -> H.HashMap [String] [Type] -> Either String (H.HashMap [String] [Type])
checkFile File{packageDec=(PackageDec (Path package)), primaryClass=primaryClass, imports=imports} types = do
  let imports' = Prelude.map (\(ImportDec (Path path)) -> path) imports
  typesToAdd <- trace "getting Class types" (getTypes package types imports' Nothing primaryClass)
  let newTypes = trace "updating types" $ H.insert (package Prelude.++ [className primaryClass]) typesToAdd types
  checkClass (trace "checking class" primaryClass) (Prelude.head typesToAdd) newTypes package imports'
  
checkClass :: Class -> Type -> H.HashMap [String] [Type] -> [String] -> [[String]] -> Either String (H.HashMap [String] [Type])
checkClass Class{members=members} classType types package imports = do
  let fieldTypes = Prelude.foldl (\memberTypes field -> case field of
                                      FieldMember (Field _ fieldName fieldType) -> H.insert ("this":fieldName:[]) [fieldType] memberTypes
                                      _ -> memberTypes) H.empty members
  let localTypes = [fieldTypes]
  let methodTypes = Prelude.foldl (\acc member -> case member of
                                      MethodMember method -> ((methodName method), (getTypes package types imports (Just classType) method)):acc
                                      _ -> acc) [] members
  out <- Left (trace ("Mesage " Prelude.++ (show methodTypes)) (show methodTypes))
  let (methodNames, methodTypes') = Prelude.unzip methodTypes
  methodTypes'' <- Prelude.sequence methodTypes' {- TODO: check subclasses -}
  let newTypes = Prelude.foldl (\acc (name, types) -> H.insert (package Prelude.++ [name]) types acc) H.empty $ Prelude.zip methodNames methodTypes''
  return $ Prelude.sequence $ Prelude.map (\method -> checkMethod method newTypes localTypes) $ Prelude.filter (\member -> case member of
    MethodMember _ -> True
    _ -> False) members
