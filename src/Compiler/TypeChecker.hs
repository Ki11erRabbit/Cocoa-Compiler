{-# LANGUAGE RecordWildCards #-}
module Compiler.TypeChecker (topLevelTypeCheck) where

import Compiler.Ast.File
import Compiler.Ast.Class
import Compiler.Ast.Method
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Data.HashMap.Strict as H
import Data.HashSet as S
import Data.Vector as V
import Debug.Trace (trace)
import Control.DeepSeq (deepseq)

emptyMapWithObject :: H.HashMap [String] [Type]
emptyMapWithObject = H.insert ["Object"] [ClassType (Path ["Object"])] H.empty

emptyMapWithoutObject :: H.HashMap [String] [Type]
emptyMapWithoutObject = H.empty


topLevelTypeCheck :: V.Vector File -> [Int] -> Either String ()
topLevelTypeCheck files order = checkFiles files order emptyMapWithObject


checkFiles :: V.Vector File -> [Int] -> H.HashMap [String] [Type] -> Either String ()
checkFiles files (next:rest) types = do
  _ <- trace (show next Prelude.++ show rest) return $ ()
  let file = files V.! next
  newTypes <- checkFile file types
  checkFiles files rest newTypes
checkFiles files [] types = Right ()

checkFile :: File -> H.HashMap [String] [Type] -> Either String (H.HashMap [String] [Type])
checkFile File{packageDec=(PackageDec (Path package)), primaryClass=primaryClass, imports=imports} types = do
  let imports' = Prelude.map (\(ImportDec (Path path)) -> path) imports
  typesToAdd <- (getTypes package types imports' Nothing primaryClass)
  let newTypes = H.insert (package Prelude.++ [className primaryClass]) typesToAdd types
  checkClass primaryClass typesToAdd newTypes package imports'
  
checkClass :: Class -> [Type] -> H.HashMap [String] [Type] -> [String] -> [[String]] -> Either String (H.HashMap [String] [Type])
checkClass Class{className=className, members=members} classTypes types package imports = do
  let fieldTypes = Prelude.foldl (\memberTypes field -> case field of
                                      FieldMember (Field _ fieldName fieldType) -> H.insert ("this":fieldName:[]) [fieldType] memberTypes
                                      _ -> memberTypes) H.empty members
  let fieldTypes' = H.insert ["this"] [Prelude.head classTypes] $ H.insert ["super"] (Prelude.tail classTypes) fieldTypes
  let localTypes = [fieldTypes']
  let methodTypes = Prelude.foldl (\acc member -> case member of
                                      MethodMember method -> ((methodName method), (getTypes package types imports (Just (Prelude.head classTypes)) method)):acc
                                      _ -> acc) [] members
  let (methodNames, methodTypes') = Prelude.unzip methodTypes
  methodTypes'' <- Prelude.sequence methodTypes' {- TODO: check subclasses and deal with overloading -} 
  let newTypes = trace "newTypes" $ Prelude.foldl (\acc (name, types) -> H.insert (package Prelude.++ className:[name]) types acc) types $ Prelude.zip methodNames methodTypes''
  let methods = Prelude.filter (\member -> case member of
                                   MethodMember _ -> True
                                   _ -> False) members
  _ <- (Prelude.foldl (\acc result -> case (acc, result) of
                                  (Left msg, Right _) -> Left msg
                                  (Right _, Right _) -> Right ()
                                  (Right _, Left msg) -> Left msg
                                  (Left msg, Left msg2) -> Left (msg Prelude.++ "\n" Prelude.++ msg2)) (Right ())
                $ Prelude.map (\(method, ty) -> checkMethod method ty newTypes localTypes)
                $ Prelude.zip methods $ Prelude.concat methodTypes'')
  return $ newTypes


checkMethod :: Member -> Type -> (H.HashMap [String] [Type]) -> [(H.HashMap [String] [Type])] -> Either String ()
checkMethod (MethodMember (Method{params=params, body=Prototype})) _ types localTypes = Right ()
checkMethod (MethodMember (Method{params=params, body=(Native _)})) _ types localTypes = Right ()
checkMethod (MethodMember (Method{params=params, body=(Redirect _)})) _ types localTypes = Right ()
checkMethod (MethodMember (Method{params=params, body=(MethodBody stmts)})) methodType types localTypes = do
                let localTypes' = (loadParams params):localTypes
                checkStatements stmts methodType types localTypes
  where
    loadParams (Param{..}:rest) = H.insert [paramName] [paramType] (loadParams rest)
    loadParams [] = H.empty
checkMethod _ _ _ _ = Left "Method was not a method"
  

checkStatements :: [Statement] -> Type -> (H.HashMap [String] [Type]) -> [(H.HashMap [String] [Type])] -> Either String ()
checkStatements (stmt:rest) methodType types localTypes = do
  localTypes' <- checkStatement stmt methodType types localTypes
  checkStatements rest methodType types localTypes'
checkStatements [] _ _ _ = Right ()


checkStatement :: Statement -> Type -> (H.HashMap [String] [Type]) -> [(H.HashMap [String] [Type])] -> Either String [(H.HashMap [String] [Type])]
checkStatement (LetStmt (LetStatement (LetVar varName) (Just ty) expr)) methodType types localTypes = do
  let exprTypes = inferExpr expr types localTypes
  if Prelude.elem ty exprTypes
    then Right ((H.insert [varName] [ty] H.empty):localTypes) else Left "Types do not match" {- TODO: add line numbers -}
checkStatement (ReturnStmt (ReturnExpr expr)) (MethodType _ _ returnType) types localTypes = do
  _ <- trace "Checking Return" $ return ()
  let exprTypes = inferExpr expr types localTypes
  if Prelude.elem returnType exprTypes
    then trace "right" $ Right localTypes else trace "left" $ Left "Return Type does not match"
checkStatement stmt methodType types localTypes = error "TODO"


{- TODO: have a hashmap that goes from types to a hashmap of types for each class so we can typecheck members and methods -}
inferExpr :: Expr -> (H.HashMap [String] [Type]) -> [(H.HashMap [String] [Type])] -> [Type]
inferExpr (Literal (IntLit _)) _ _ = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)] 
inferExpr (Literal (FloatLit _)) _ _ = [(Primitive F32PrimType), (Primitive F64PrimType)] 
inferExpr (Literal (StringLit _)) _ _ = [ClassType (Path ["String"] )]
inferExpr (Literal (BoolLit _)) _ _ = [Primitive BoolPrimType]
inferExpr (Literal (CharLit _)) _ _ = [Primitive CharPrimType]
inferExpr ThisExpr _ localTypes = lookupLocal localTypes ["this"]
inferExpr SuperExpr _ localTypes = lookupLocal localTypes ["super"]
inferExpr (Paren expr) types localTypes = inferExpr expr types localTypes
inferExpr NullExpr _ _ = [ClassType (Path ["Object"])]
inferExpr (Var var) _ localTypes = lookupLocal localTypes [var]
inferExpr (FieldAccess ThisExpr name) types localTypes = lookupLocal localTypes ["this",name]

--inferExpr expr types localTypes

lookupLocal :: [H.HashMap [String] [Type]] -> [String] -> [Type]
lookupLocal (current:rest) var = case current H.!? var of
  Just x -> x
  Nothing -> lookupLocal rest var
lookupLocal [] _ = []
