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

type Package = [String]
type Imports = [[String]]

emptyMapWithObject :: H.HashMap [String] [Type]
emptyMapWithObject = H.insert ["Object"] [ClassType (Path ["Object"])] H.empty

emptyMapWithoutObject :: H.HashMap [String] [Type]
emptyMapWithoutObject = H.empty


topLevelTypeCheck :: V.Vector File -> [Int] -> Either String (V.Vector File)
topLevelTypeCheck files order = checkFiles files order emptyMapWithObject H.empty


checkFiles :: V.Vector File -> [Int] -> TypeMap -> ObjectMemberTypes -> Either String (V.Vector File)
checkFiles files (next:rest) types omt = do
  _ <- trace (show next Prelude.++ show rest) return $ ()
  let file = files V.! next
  (file', newTypes, newOMT) <- checkFile file types omt
  let files' = V.update files $ V.singleton (next, file')
  checkFiles files' rest newTypes newOMT
checkFiles files [] _ _ = Right files

checkFile :: File -> TypeMap -> ObjectMemberTypes -> Either String (File, TypeMap, ObjectMemberTypes)
checkFile File{packageDec=(PackageDec (Path package)), primaryClass=primaryClass, imports=imports} types omt = do
  let imports' = Prelude.map (\(ImportDec (Path path)) -> path) imports
  typesToAdd <- (getTypes package types imports' Nothing primaryClass)
  let newTypes = H.insert (package Prelude.++ [className primaryClass]) typesToAdd types
  (class', types', omt') <- checkClass primaryClass typesToAdd newTypes omt (package, imports')
  Right (File (PackageDec (Path package)) imports class', types', omt')
  
  
checkClass :: Class -> [Type] -> TypeMap -> ObjectMemberTypes -> (Package, Imports) -> Either String (Class, TypeMap, ObjectMemberTypes)
checkClass class' classTypes types omt (package, imports) =
  let Class{className=className, members=members} = class' in do
    let oMTValue = Prelude.foldl (\memberTypes field -> case field of
                                     FieldMember (Field _ fieldName fieldType) ->
                                       H.insert (fieldName:[]) [fieldType] memberTypes
                                     _ -> memberTypes)
                   H.empty members
    let objectVars = H.insert ["this"] [Prelude.head classTypes]
                     $ H.insert ["super"] [Prelude.head $ Prelude.tail classTypes] H.empty
    let localTypes = [objectVars]
    let methodTypes = Prelude.foldl (\acc member -> case member of
                                        MethodMember method -> ((methodName method), (getTypes package types imports (Just (Prelude.head classTypes)) method)):acc
                                        _ -> acc)
          [] members
    let (methodNames, methodTypes') = Prelude.unzip methodTypes
    methodTypes'' <- Prelude.sequence methodTypes' {- TODO: check subclasses and deal with overloading -}
    let oMTValue' = Prelude.foldl (\acc (name, types) ->
                                                case acc H.!? [name] of
                                                  Just x -> H.insert [name] (types Prelude.++ x) acc
                                                  Nothing -> H.insert [name] types acc)
                               oMTValue $ Prelude.zip methodNames methodTypes''
    let omtValue'' = (Direct oMTValue'):(Prelude.map SuperRef $ Prelude.tail classTypes)
    let newOMT = H.insert (Prelude.head classTypes) omtValue'' omt
    let methods = Prelude.filter methodFilter members
    methods' <- Prelude.sequence $ Prelude.map (\(method, ty) -> checkMethod method ty types localTypes newOMT) $ Prelude.zip methods $ Prelude.concat methodTypes''
    let fields = (Prelude.filter filterOutMethods members) Prelude.++ methods'
    return $ (class' {members = fields},types, newOMT)
  where
    methodFilter (MethodMember _) = True
    methodFilter _ = False
    filterOutMethods (MethodMember _) = False
    filterOutMethods _ = True
    


checkMethod :: Member -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String Member
checkMethod (MethodMember method) methodType types localTypes omt =
  case method of
    (Method{params=params, body=(MethodBody stmts)}) -> do
      let localTypes' = (loadParams params):localTypes
      stmts' <- checkStatements stmts methodType types localTypes omt []
      Right (MethodMember (method { body = MethodBody stmts' }))
    _ -> Right $ MethodMember method 
  where
    loadParams (Param{..}:rest) = H.insert [paramName] [paramType] (loadParams rest)
    loadParams [] = H.empty
checkMethod _ _ _ _ _ = Left "Method was not a method"
  

checkStatements :: [Statement] -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> [Statement] -> Either String [Statement]
checkStatements (stmt:rest) methodType types localTypes omt acc = do
  (stmt', localTypes') <- checkStatement stmt rest methodType types localTypes omt
  checkStatements rest methodType types localTypes' omt (stmt':acc)
checkStatements [] _ _ _ _ stmts = Right $ Prelude.reverse stmts


checkStatement :: Statement -> [Statement] -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String (Statement, [TypeMap])
checkStatement (LetStmt (LetStatement (LetVar varName (Just ty)) expr)) _ methodType types localTypes omt = do
  (expr', exprTypes) <- inferExpr expr methodType types localTypes omt
  if Prelude.elem ty exprTypes
    then Right (LetStmt (LetStatement (LetVar varName (Just ty)) expr'), ((H.insert [varName] [ty] H.empty):localTypes)) else Left "Types do not match" {- TODO: add line numbers -}
checkStatement (ReturnStmt (ReturnExpr expr)) _ (MethodType mx my returnType) types localTypes omt = do
  (expr', exprTypes) <- inferExpr expr (MethodType mx my returnType) types localTypes omt
  if Prelude.elem returnType exprTypes
    then Right (ReturnStmt (ReturnExpr expr'), localTypes) else Left "Return Type does not match"
checkStatement (Hanging expr) _ (MethodType mx my returnType) types localTypes omt = do
  (expr', exprTypes) <- inferExpr expr (MethodType mx my returnType) types localTypes omt
  if Prelude.elem returnType exprTypes
    then Right (Hanging expr', localTypes) else Left "Return Type does not match"
checkStatement (ReturnStmt ReturnUnit) _ (MethodType _ _ returnType) types localTypes omt =
  case returnType of
    (Primitive UnitPrimType) -> Right (ReturnStmt (ReturnUnit), localTypes)
    _ -> Left "Return type does not match"
checkStatement (WhileStmt (WhileStatement test body)) restStmts methodType types localTypes omt = do
  (test', testExprs) <- inferExpr test methodType types localTypes omt
  if not $ Prelude.elem (Primitive BoolPrimType) testExprs
    then Left "Test expression for while was not a boolean"
    else do
    body' <- checkStatements body methodType types localTypes omt []
    Right $ (WhileStmt (WhileStatement test' body'), localTypes)
checkStatement (IfStmt ifExpr) restStmts methodType types localTypes omt = do
  ifExpr' <- checkIfExpr ifExpr
  Right (IfStmt ifExpr', localTypes)
  where
    checkIfExpr (IfExpr test thenBody Nothing) = do
      (test', testExprs) <- inferExpr test methodType types localTypes omt
      if not $ Prelude.elem (Primitive BoolPrimType) testExprs
        then Left "Test expression for if was not a boolean"
        else do
        body' <- checkStatements thenBody methodType types localTypes omt []
        Right $ IfExpr test' body' Nothing
    checkIfExpr (IfExpr test thenBody (Just (Left elseBody))) = do
      (test', testExprs) <- inferExpr test methodType types localTypes omt
      if not $ Prelude.elem (Primitive BoolPrimType) testExprs
        then Left "Test expression for if was not a boolean"
        else do
        thenBody' <- checkStatements thenBody methodType types localTypes omt []
        elseBody' <- checkStatements elseBody methodType types localTypes omt []
        Right $ IfExpr test thenBody' (Just (Left elseBody'))
    checkIfExpr (IfExpr test thenBody (Just (Right ifExr))) = do
      (test', testExprs) <- inferExpr test methodType types localTypes omt
      if not $ Prelude.elem (Primitive BoolPrimType) testExprs
        then Left "Test expression for if was not a boolean"
        else do
        thenBody' <- checkStatements thenBody methodType types localTypes omt []
        ifExpr' <- checkIfExpr ifExpr
        Right $ IfExpr test' thenBody' (Just (Right ifExpr'))
checkStatement (AssignmentStmt expr1 expr2) _ methodType types localTypes omt = do
  (expr1', expr1Types) <- inferExpr expr1 methodType types localTypes omt
  (expr2', expr2Types) <- inferExpr expr2 methodType types localTypes omt
  if expr2Types `contains` expr1Types then
    Right (AssignmentStmt expr1' expr2', localTypes)
    else
    Left "Types in assignment don't match"
checkStatement stmt restStmts methodType types localTypes omt = error "TODO"


inferExpr :: Expr -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String (Expr, [Type])
inferExpr (Literal (IntLit x)) _ _ _ _ = return ((Literal (IntLit x)), [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]) 
inferExpr (Literal (FloatLit x)) _ _ _ _ = return ((Literal (FloatLit x)), [(Primitive F32PrimType), (Primitive F64PrimType)]) 
inferExpr (Literal (StringLit x)) _ _ _ _ = return (Literal (StringLit x), [ClassType (Path ["String"] )])
inferExpr (Literal (BoolLit x)) _ _ _ _ = return (Literal (BoolLit x), [Primitive BoolPrimType])
inferExpr (Literal (CharLit x)) _ _ _ _ = return (Literal (CharLit x), [Primitive CharPrimType])
inferExpr ThisExpr _ _ localTypes omt = return (ThisExpr, lookupLocal localTypes ["this"])
inferExpr SuperExpr _ _ localTypes omt = return (SuperExpr, lookupLocal localTypes ["super"])
inferExpr (Paren expr) methodType types localTypes omt = do
  (expr', types') <- inferExpr expr methodType types localTypes omt
  Right ((Paren expr'), types')
inferExpr NullExpr _ _ _ _ = return (NullExpr, [ClassType (Path ["Object"])])
inferExpr (Var var) _ _ localTypes _ = return ((Var var), lookupLocal localTypes [var])
inferExpr (Cast (Primitive ty) expr) methodType types localTypes omt = do
  (expr', exprTypes) <- inferExpr expr methodType types localTypes omt
  if exprTypes `contains` [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
    then Right (Cast (Primitive ty) expr', [(Primitive ty)])
    else Left "Couldn't typecheck a primitive cast"
inferExpr (Cast ty expr) _ types localTypes omt = Right (Cast ty expr, [ty]) -- TODO: Warn user about casting as well as make sure that it is only object types
inferExpr (FieldAccess expr name) methodType types localTypes omt = do
  (expr', exprTypes) <- inferExpr expr methodType types localTypes omt
  case exprTypes of
    (ty:[]) -> Right (FieldAccess expr' name, (getClassMembers ty omt) H.! [name])
    _ -> Left "Too many possibilities for field access"
inferExpr (Call body argExprs) methodType types localTypes omt = do
  (body', methodTypes) <- inferExpr body methodType types localTypes omt
  out <- Prelude.sequence $ Prelude.map (\arg -> inferExpr arg methodType types localTypes omt) argExprs
  let (argExprs', argExprTypes) = Prelude.unzip out
  types' <- checkMethodType methodTypes argExprTypes
  Right (Call body' argExprs', types')
  where
    checkMethodType :: [Type] -> [[Type]] -> Either String [Type]
    checkMethodType (method:rest) args = do
      if checkMethodArgsStart method args
        then case method of
               MethodType _ _ retType -> Right [retType]
               _ -> Left "field was not a method"
        else checkMethodType rest args
    checkMethodType [] args = Left "No methods match type signature"
    checkMethodArgsStart :: Type -> [[Type]] -> Bool
    checkMethodArgsStart (MethodType _ (argTy:xs) _) (arg:ys) = Prelude.elem argTy arg && checkMethodArgs xs ys
    checkMethodArg (argTy:xs) (arg:ys) = Prelude.elem argTy arg && checkMethodArgs xs ys
    checkMethodArgs [] [] = True
    checkMethodArgs [] _ = False
inferExpr (UnaryOp Neg (Literal (IntLit x))) _ _ _ _ = return ((UnaryOp Neg (Literal (IntLit x))), [(Primitive I8PrimType), (Primitive I16PrimType), (Primitive I32PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)])
inferExpr (UnaryOp Neg (Literal (FloatLit x))) _ _ _ _ = return ((UnaryOp Neg (Literal (FloatLit x))), [(Primitive F32PrimType), (Primitive F64PrimType)])
inferExpr (UnaryOp Neg expr) methodType types localTypes omt = do
  (expr', exprTypes) <- inferExpr expr methodType types localTypes omt
  if exprTypes `contains` [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)] {- TODO: add support for inferface impl -}
    then Right (expr', exprTypes)
    else Left "Operand for - (unary) is an invalid type"
inferExpr (BinaryOp op expr1 expr2) methodType types localTypes omt = checkBinOp op expr1 expr2 methodType types localTypes omt
inferExpr (IfExprExpr (IfExpr test thenBody elsePart)) methodType types localTypes omt = do
  (ifExpr', outTypes) <- inferIfExpr (IfExpr test thenBody elsePart) []
  Right (IfExprExpr ifExpr', outTypes)
  where
    inferIfExpr :: IfExpr -> [Type] -> Either String (IfExpr, [Type]) 
    inferIfExpr (IfExpr test thenBody Nothing) [] = Left "if expression requires else block"
    inferIfExpr (IfExpr test thenBody elsePart) [] = do
      (test', testExprs) <- inferExpr test methodType types localTypes omt
      if not $ Prelude.elem (Primitive BoolPrimType) testExprs
        then Left "Test expression for if was not a boolean"
        else do
        (thenBody', thenBodyTypes) <- inferStatements thenBody methodType types localTypes omt
        (elsePart', returnTypes) <- handleElsePart elsePart thenBodyTypes
        Right (IfExpr test' thenBody' elsePart', returnTypes)
    inferIfExpr (IfExpr test thenBody elsePart) returnTypes = do
      (test', testExprs) <- inferExpr test methodType types localTypes omt
      if not $ Prelude.elem (Primitive BoolPrimType) testExprs
        then Left "Test expression for if was not a boolean"
        else do
        (thenBody', thenBodyTypes) <- inferStatements thenBody methodType types localTypes omt
        (elsePart', returnTypes') <- handleElsePart elsePart $ intersect thenBodyTypes returnTypes
        Right (IfExpr test' thenBody' elsePart', returnTypes')
    handleElsePart :: (Maybe (Either [Statement] IfExpr)) -> [Type] -> Either String (Maybe (Either [Statement] IfExpr), [Type])
    handleElsePart (Just (Left elseBody)) returnTypes = do
      (elseBody', elseBodyTypes) <- inferStatements elseBody methodType types localTypes omt
      let intersection = intersect returnTypes elseBodyTypes
      if Prelude.length intersection == 0 then
        Left "If expression body types don't match"
        else
        Right ((Just (Left elseBody')), intersection)
    handleElsePart (Just (Right ifExpr)) returnTypes = do
      (ifExpr', returnTypes') <- inferIfExpr ifExpr returnTypes
      Right (Just (Right ifExpr'), returnTypes')
      
  

--inferExpr expr types localTypes

checkBinOp :: BinaryOp -> Expr -> Expr -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String (Expr, [Type])
checkBinOp LogicalAnd expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if Prelude.elem (Primitive BoolPrimType) leftList && Prelude.elem (Primitive BoolPrimType) rightList
      then Right (BinaryOp LogicalAnd expr1' expr2', [Primitive BoolPrimType])
      else Left "Operands for && must be bools"
checkBinOp LogicalOr expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if Prelude.elem (Primitive BoolPrimType) leftList && Prelude.elem (Primitive BoolPrimType) rightList
      then Right (BinaryOp LogicalOr expr1' expr2', [Primitive BoolPrimType])
      else Left "Operands for || must be bools"
checkBinOp Eq expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` eqOpTypes && rightList `contains` eqOpTypes
      then Right (BinaryOp Eq expr1' expr2', [(Primitive BoolPrimType)])
      else Left "Operands for == must be Numbers, Chars, or Booleans(for now)"
  where
    eqOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType), (Primitive CharPrimType), (Primitive BoolPrimType)]
checkBinOp Neq expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` neqOpTypes && rightList `contains` neqOpTypes
      then Right (BinaryOp Neq expr1' expr2', [(Primitive BoolPrimType)])
      else Left "Operands for != must be Numbers, Chars, or Booleans(for now)"
  where
    neqOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType), (Primitive CharPrimType), (Primitive BoolPrimType)]
checkBinOp Lt expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` ltOpTypes && rightList `contains` ltOpTypes
      then Right (BinaryOp Lt expr1' expr2', [(Primitive BoolPrimType)])
      else Left "Operands for < must be Numbers or Chars (for now)"
  where
    ltOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType), (Primitive CharPrimType)]
checkBinOp Gt expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` gtOpTypes && rightList `contains` gtOpTypes
      then Right (BinaryOp Gt expr1' expr2', [(Primitive BoolPrimType)])
      else Left "Operands for > must be Numbers or Chars (for now)"
  where
    gtOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType), (Primitive CharPrimType)]
checkBinOp Le expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` leOpTypes && rightList `contains` leOpTypes
      then Right (BinaryOp Le expr1' expr2', [(Primitive BoolPrimType)])
      else Left "Operands for >= must be Numbers or Chars (for now)"
  where
    leOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType), (Primitive CharPrimType)]
checkBinOp Ge expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` geOpTypes && rightList `contains` geOpTypes
      then Right (BinaryOp Ge expr1' expr2', [(Primitive BoolPrimType)])
      else Left "Operands for >= must be Numbers or Chars (for now)"
  where
    geOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType), (Primitive CharPrimType)]
checkBinOp Add expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` addOpTypes && rightList `contains` addOpTypes
      then Right (BinaryOp Add expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for + must be Numbers (for now)"
  where
    addOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
checkBinOp Sub expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` subOpTypes && rightList `contains` subOpTypes
      then Right (BinaryOp Sub expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for - must be Numbers (for now)"
  where
    subOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
checkBinOp Mul expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` mulOpTypes && rightList `contains` mulOpTypes
      then Right (BinaryOp Mul expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for * must be Numbers (for now)"
  where
    mulOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
checkBinOp Div expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` divOpTypes && rightList `contains` divOpTypes
      then Right (BinaryOp Div expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for / must be Numbers (for now)"
  where
    divOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
checkBinOp Mod expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` modOpTypes && rightList `contains` modOpTypes
      then Right (BinaryOp Mod expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for % must be Numbers (for now)"
  where
    modOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType), (Primitive F32PrimType), (Primitive F64PrimType)]
checkBinOp BitAnd expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` andOpTypes && rightList `contains` andOpTypes
      then Right (BinaryOp BitAnd expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for & must be Integers (for now)"
  where
    andOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType)]
checkBinOp BitOr expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` orOpTypes && rightList `contains` orOpTypes
      then Right (BinaryOp BitOr expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for | must be Integers (for now)"
  where
    orOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType)]
checkBinOp BitXor expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` xorOpTypes && rightList `contains` xorOpTypes
      then Right (BinaryOp BitXor expr1' expr2', if Prelude.length leftList < Prelude.length rightList then leftList else rightList)
      else Left "Operands for ^ must be Integers (for now)"
  where
    xorOpTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType)]
checkBinOp LShift expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` opTypes && Prelude.elem amountType rightList
      then Right (BinaryOp LShift expr1' expr2', leftList)
      else Left "Operands for << must be Integers (for now)"
  where
    opTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType)]
    amountType = Primitive U32PrimType
checkBinOp RShift expr1 expr2 methodType types localTypes omt = do
  let expr1Types = inferExpr expr1 methodType types localTypes omt
  let expr2Types = inferExpr expr2 methodType types localTypes omt
  case (expr1Types, expr2Types) of
    (Left leftMsg, Left rightMsg) -> Left (leftMsg Prelude.++ "\n" Prelude.++ rightMsg)
    (Left msg, _) -> Left msg
    (_, Left msg) -> Left msg
    (Right (expr1', leftList), Right (expr2', rightList)) ->
      if leftList `contains` opTypes && Prelude.elem amountType rightList
      then Right (BinaryOp RShift expr1' expr2', leftList)
      else Left "Operands for >> must be Integers (for now)"
  where
    opTypes = [(Primitive U8PrimType), (Primitive I8PrimType), (Primitive U16PrimType), (Primitive I16PrimType), (Primitive U32PrimType), (Primitive I32PrimType), (Primitive U64PrimType), (Primitive I64PrimType)]
    amountType = Primitive U32PrimType



inferStatements :: [Statement] -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String ([Statement], [Type])
inferStatements statements methodType types localTypes omt = inferStatementsLoop statements localTypes [Primitive UnitPrimType] []
  where
    inferStatementsLoop (stmt:rest) locTypes returnTypes acc = do
      (stmt', outTypes, localTypes') <- inferStatement stmt rest methodType types locTypes omt
      inferStatementsLoop rest localTypes' outTypes (stmt':acc)
    inferStatementsLoop [] _ returnTypes acc = Right (Prelude.reverse acc, returnTypes)


inferStatement :: Statement -> [Statement] -> Type -> TypeMap -> [TypeMap] -> ObjectMemberTypes -> Either String (Statement, [Type], [TypeMap])
inferStatement (Hanging expr) _ methodType types localTypes omt = do
  (expr', outTypes) <- inferExpr expr methodType types localTypes omt
  Right (Hanging expr', outTypes, localTypes)
inferStatement stmt stmts methodType types localTypes omt = do
  (stmt', localTypes') <- checkStatement stmt stmts methodType types localTypes omt
  Right (stmt', [Primitive UnitPrimType], localTypes')



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

intersect :: Eq a => [a] -> [a] -> [a]
intersect [] [] = []
intersect [] _ = []
intersect _ [] = []
intersect right left = Prelude.filter (\x -> x `Prelude.elem` left) right

getClassMembers :: Type -> ObjectMemberTypes -> TypeMap
getClassMembers ty omt = case omt H.! ty of
  (SuperRef ty'):_ -> error "missing direct class"
  (Direct tm):_ -> tm

