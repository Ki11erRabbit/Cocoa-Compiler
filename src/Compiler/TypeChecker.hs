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

data OMPValue = SuperRef Type | Direct TypeMap 

type ObjectMemberTypes = H.HashMap Type [OMPValue] 
type TypeMap = H.HashMap [String] [Type]

emptyMapWithObject :: H.HashMap [String] [Type]
emptyMapWithObject = H.insert ["Object"] [ClassType (Path ["Object"])] H.empty

emptyMapWithoutObject :: H.HashMap [String] [Type]
emptyMapWithoutObject = H.empty


topLevelTypeCheck :: V.Vector File -> [Int] -> Either String ()
topLevelTypeCheck files order = checkFiles files order emptyMapWithObject H.empty


checkFiles :: V.Vector File -> [Int] -> TypeMap -> ObjectMemberTypes -> Either String ()
checkFiles files (next:rest) types omt = do
  _ <- trace (show next Prelude.++ show rest) return $ ()
  let file = files V.! next
  (newTypes, newOMT) <- checkFile file types omt
  checkFiles files rest newTypes newOMT
checkFiles files [] types omt = Right ()

checkFile :: File -> TypeMap -> ObjectMemberTypes -> Either String (TypeMap, ObjectMemberTypes)
checkFile File{packageDec=(PackageDec (Path package)), primaryClass=primaryClass, imports=imports} types omt = do
  let imports' = Prelude.map (\(ImportDec (Path path)) -> path) imports
  typesToAdd <- (getTypes package types imports' Nothing primaryClass)
  let newTypes = H.insert (package Prelude.++ [className primaryClass]) typesToAdd types
  checkClass primaryClass typesToAdd newTypes omt package imports'
  
checkClass :: Class -> [Type] -> TypeMap -> ObjectMemberTypes -> [String] -> [[String]] -> Either String (TypeMap, ObjectMemberTypes)
checkClass Class{className=className, members=members} classTypes types omt package imports = do
  let classDirectOMTValue = Prelude.foldl (\memberTypes field -> case field of
                                      FieldMember (Field _ fieldName fieldType) -> H.insert (fieldName:[]) [fieldType] memberTypes
                                      _ -> memberTypes) H.empty members
  let fieldTypes = H.insert ["this"] [Prelude.head classTypes] $ H.insert ["super"] [Prelude.head $ Prelude.tail classTypes] H.empty
  let localTypes = [fieldTypes]
  let methodTypes = Prelude.foldl (\acc member -> case member of
                                      MethodMember method -> ((methodName method), (getTypes package types imports (Just (Prelude.head classTypes)) method)):acc
                                      _ -> acc) [] members
  let (methodNames, methodTypes') = Prelude.unzip methodTypes
  methodTypes'' <- Prelude.sequence methodTypes' {- TODO: check subclasses and deal with overloading -}
  let classDirectOMTValue' = Prelude.foldl (\acc (name, types) ->
                                              case acc H.!? [name] of
                                                Just x -> H.insert [name] (types Prelude.++ x) acc
                                                Nothing -> H.insert [name] types acc)
                             classDirectOMTValue $ Prelude.zip methodNames methodTypes''
  let omtValue = (Direct classDirectOMTValue):(Prelude.map SuperRef $ Prelude.tail classTypes)
  let newOMT = H.insert (Prelude.head classTypes) omtValue omt
  let methods = Prelude.filter (\member -> case member of
                                   MethodMember _ -> True
                                   _ -> False) members
  _ <- (Prelude.foldl (\acc result -> case (acc, result) of
                                  (Left msg, Right _) -> Left msg
                                  (Right _, Right _) -> Right ()
                                  (Right _, Left msg) -> Left msg
                                  (Left msg, Left msg2) -> Left (msg Prelude.++ "\n" Prelude.++ msg2)) (Right ())
                $ Prelude.map (\(method, ty) -> checkMethod method ty types localTypes newOMT)
                $ Prelude.zip methods $ Prelude.concat methodTypes'')
  return $ (types, newOMT)


checkMethod :: Member -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String ()
checkMethod (MethodMember (Method{params=params, body=Prototype})) _ types localTypes _ = Right ()
checkMethod (MethodMember (Method{params=params, body=(Native _)})) _ types localTypes _ = Right ()
checkMethod (MethodMember (Method{params=params, body=(Redirect _)})) _ types localTypes _ = Right ()
checkMethod (MethodMember (Method{params=params, body=(MethodBody stmts)})) methodType types localTypes omt = do
                let localTypes' = (loadParams params):localTypes
                checkStatements stmts methodType types localTypes omt
  where
    loadParams (Param{..}:rest) = H.insert [paramName] [paramType] (loadParams rest)
    loadParams [] = H.empty
checkMethod _ _ _ _ _ = Left "Method was not a method"
  

checkStatements :: [Statement] -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String ()
checkStatements (stmt:rest) methodType types localTypes omt = do
  localTypes' <- checkStatement stmt methodType types localTypes omt
  checkStatements rest methodType types localTypes' omt
checkStatements [] _ _ _ _ = Right ()


checkStatement :: Statement -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String [TypeMap]
checkStatement (LetStmt (LetStatement (LetVar varName) (Just ty) expr)) methodType types localTypes omt = do
  exprTypes <- inferExpr expr types localTypes omt
  if Prelude.elem ty exprTypes
    then Right ((H.insert [varName] [ty] H.empty):localTypes) else Left "Types do not match" {- TODO: add line numbers -}
checkStatement (ReturnStmt (ReturnExpr expr)) (MethodType _ _ returnType) types localTypes omt = do
  exprTypes <- inferExpr expr types localTypes omt
  if Prelude.elem returnType exprTypes
    then Right localTypes else Left "Return Type does not match"
checkStatement (ReturnStmt ReturnUnit) (MethodType _ _ returnType) types localTypes omt =
  case returnType of
    (Primitive UnitPrimType) -> Right localTypes
    _ -> Left "Return type does not match"
checkStatement stmt methodType types localTypes omt = error "TODO"


inferExpr :: Expr -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String [Type]
inferExpr (Literal (IntLit _)) _ _ _ = return [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)] 
inferExpr (Literal (FloatLit _)) _ _ _ = return [(Primitive F32PrimType), (Primitive F64PrimType)] 
inferExpr (Literal (StringLit _)) _ _ _ = return [ClassType (Path ["String"] )]
inferExpr (Literal (BoolLit _)) _ _ _ = return [Primitive BoolPrimType]
inferExpr (Literal (CharLit _)) _ _ _ = return [Primitive CharPrimType]
inferExpr ThisExpr _ localTypes omt = return $ lookupLocal localTypes ["this"]
inferExpr SuperExpr _ localTypes omt = return $ lookupLocal localTypes ["super"]
inferExpr (Paren expr) types localTypes omt = inferExpr expr types localTypes omt
inferExpr NullExpr _ _ _ = return [ClassType (Path ["Object"])]
inferExpr (Var var) _ localTypes _ = return $ lookupLocal localTypes [var]
inferExpr (Cast (Primitive ty) expr) types localTypes omt = do
  exprTypes <- inferExpr expr types localTypes omt
  if exprTypes `contains` [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
    then Right [(Primitive ty)]
    else Left "Couldn't typecheck a primitive cast"
inferExpr (Cast ty expr) types localTypes omt = do
  error "casting for other types isn't implemented yet"
inferExpr (FieldAccess expr name) types localTypes omt = do
  exprTypes <- inferExpr expr types localTypes omt
  case exprTypes of
    (ty:[]) -> Right $ (getClassMembers ty omt) H.! [name]
    _ -> Left "Too many possibilities for field access"
inferExpr (Call body argExprs) types localTypes omt = do
  methodTypes <- inferExpr body types localTypes omt
  argExprTypes <- Prelude.sequence $ Prelude.map (\arg -> inferExpr arg types localTypes omt) argExprs
  checkMethodType methodTypes argExprTypes
  where
    checkMethodType (method:rest) args = do
      if checkMethodArgs method args
        then case method of
               MethodType _ _ retType -> Right [retType]
               _ -> Left "field was not a method"
        else checkMethodType rest args
    checkMethodType [] args = Left "No methods match type signature"
    checkMethodArgs (MethodType _ (argTy:xs) _) (arg:ys) = Prelude.elem argTy arg && checkMethodArgs xs ys
    checkMethodArgs (MethodType _ [] _) [] = True
    checkMethodArgs (MethodType _ _ _) _ = False
    
        
inferExpr (UnaryOp Neg (Literal (IntLit _))) _ _ _ = return [(Primitive I8PrimType), (Primitive I16PrimType), (Primitive I32PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
inferExpr (UnaryOp Neg (Literal (FloatLit _))) _ _ _ = return [(Primitive F32PrimType), (Primitive F64PrimType)]
inferExpr (UnaryOp Neg expr) types localTypes omt = do
  exprTypes <- inferExpr expr types localTypes omt
  if exprTypes `contains` [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)] {- TODO: add support for inferface impl -}
    then Right exprTypes
    else Left "Operand for - (unary) is an invalid type"
inferExpr (BinaryOp LogicalAnd expr1 expr2) types localTypes omt = case ((inferExpr expr1 types localTypes omt), (inferExpr expr1 types localTypes omt)) of
  (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
  (Left msg, _) -> Left msg
  (_, Left msg) -> Left msg
  (Right leftList, Right rightList) -> if Prelude.elem (Primitive BoolPrimType) leftList && Prelude.elem (Primitive BoolPrimType) rightList
    then Right [Primitive BoolPrimType]
    else Left "Operands for && must be bools"
inferExpr (BinaryOp LogicalOr expr1 expr2) types localTypes omt = case ((inferExpr expr1 types localTypes omt), (inferExpr expr1 types localTypes omt)) of
  (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
  (Left msg, _) -> Left msg
  (_, Left msg) -> Left msg
  (Right leftList, Right rightList) -> if Prelude.elem (Primitive BoolPrimType) leftList && Prelude.elem (Primitive BoolPrimType) rightList
    then Right [Primitive BoolPrimType]
    else Left "Operands for || must be bools"

--inferExpr expr types localTypes

lookupLocal :: [H.HashMap [String] [Type]] -> [String] -> [Type]
lookupLocal (current:rest) var = case current H.!? var of
  Just x -> x
  Nothing -> lookupLocal rest var
lookupLocal [] _ = []


contains :: Eq a => [a] -> [a] -> Bool
contains source (check:rest) = if (Prelude.elem check source)
  then True
  else contains source rest
contains _ [] = False


getClassMembers :: Type -> ObjectMemberTypes -> TypeMap
getClassMembers ty omt = case omt H.! ty of
  (SuperRef ty'):_ -> error "missing direct class"
  (Direct tm):_ -> tm

