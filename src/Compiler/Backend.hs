module Compiler.Backend (compile) where

import Compiler.SSA.File as SSAFile
import Compiler.Ast.File as AstFile
import Compiler.Ast.Shared as AstShared
import Compiler.SSA.Class as SSAClass
import Compiler.Ast.Class as AstClass
import Compiler.Ast.Method as AstMethod
import Compiler.SSA.Method as SSAMethod
import Compiler.Backend.Bytecode
import Compiler.Backend.ClassFile
import Control.Monad.State as St
import Data.List
import Data.Int
import Data.Bits
import Data.Vector as V
import Data.HashMap.Strict as H

data BackendState = BackendState
  { currentClassName :: String
  , constantPool :: [PoolEntry]
  , thisClassLocation :: Integer
  , superClassLocation :: Integer
  , classFlags :: Int8
  , fieldInfo :: [FieldInfo]
  , methodInfo :: [MethodInfo]
  , interfaceInfo :: [InterfaceInfo]
  , stringLocations :: [Integer]
  , previousClassDeclarations :: H.HashMap String ClassFile
  , methodNamesToIndex :: H.HashMap String Integer
  , methodNamesToInfo :: H.HashMap String MethodInfo
  , stringsToIndex :: H.HashMap String Integer
  } deriving (Show, Eq)

type Backend a = St.State BackendState a

getClassFile :: String -> Backend ClassFile
getClassFile className = do
  state <- get
  case H.lookup className (previousClassDeclarations state) of
    Just classFile -> return classFile
    Nothing -> error $ "Class not found: " Prelude.++ className

-- TODO: Write a function that resets the State and turns it into a ClassFile and returns it/puts it in the hashmap
buildClassFile :: String -> Backend ClassFile
buildClassFile className = do
  state <- get
  let classFile = ClassFile (thisClassLocation state) (superClassLocation state) (Compiler.Backend.classFlags state) (Prelude.reverse $ Compiler.Backend.constantPool state) (Prelude.reverse $ interfaceInfo state) (Prelude.reverse $ fieldInfo state) (Prelude.reverse $ methodInfo state) (Prelude.reverse $ stringLocations state)
  put $ BackendState [] 0 0 0 [] [] [] (H.insert className classFile (previousClassDeclarations state)) (methodNamesToIndex state) H.empty
  return classFile

setCurrentClassName :: String -> Backend ()
setCurrentClassName name = St.modify $ \state -> state { currentClassName = name }

getCurrentClassName :: Backend String
getCurrentClassName = do
  state <- St.get
  return $ currentClassName state

setThisClass :: ClassInfo -> Backend ()
setThisClass info = do
  state <- St.get
  St.put $ state { thisClassLocation = Prelude.length (Compiler.Backend.constantPool state) }
  St.modify $ \state -> state { Compiler.Backend.constantPool = (ClassInfoEntry info):(Compiler.Backend.constantPool state) }

setSuperClass :: ClassInfo -> Backend ()
setSuperClass info = do
  state <- St.get
  St.put $ state { superClassLocation = Prelude.length (Compiler.Backend.constantPool state) }
  St.modify $ \state -> state { Compiler.Backend.constantPool = (ClassInfoEntry info):(Compiler.Backend.constantPool state) }

setClassFlags :: Int8 -> Backend ()
setClassFlags flags = St.modify $ \state -> state { Compiler.Backend.classFlags = flags }

addField :: FieldInfo -> Backend ()
addField info = do
  state <- St.get
  St.modify $ \state -> state { fieldInfo = info:(fieldInfo state) }

addMethod :: String -> MethodEntryType -> Backend Int
addMethod name method = do
  state <- St.get
  let index = Prelude.length (methodInfo state)
  St.put $ state { methodNamesToIndex = H.insert name index (methodNamesToIndex state) }
  St.modify $ \state -> state { Compiler.Backend.constantPool = (MethodEntry method):(Compiler.Backend.constantPool state) }
  return index

updateMethod :: Int -> MethodEntryType -> Backend ()
updateMethod index method = do
  state <- St.get
  let constantPool' = Prelude.reverse $ Compiler.Backend.constantPool state
  let constantPool'' = (Prelude.take index constantPool') Prelude.++ (MethodEntry method):(Prelude.drop (index + 1) constantPool')
  St.put $ state { Compiler.Backend.constantPool = Prelude.reverse constantPool'' }
  
  

getMethodIndex :: String -> Backend Int
getMethodIndex name = do
  state <- St.get
  case H.lookup name (methodNamesToIndex state) of
    Just index -> return index
    Nothing -> error $ "Method not found: " Prelude.++ name

addMethodInfo :: MethodInfo -> Backend ()
addMethodInfo info = do
  state <- St.get
  St.modify $ \state -> state { methodInfo = info:(methodInfo state) }
  St.modify $ \state -> state { methodNamesToInfo = H.insert (Compiler.Backend.ClassFile.methodName info) info (methodNamesToInfo state) }

addInterfaceInfo :: InterfaceInfo -> Backend ()
addInterfaceInfo info = do
  state <- St.get
  St.modify $ \state -> state { interfaceInfo = info:(interfaceInfo state) }

addDataString :: String -> Backend Int
addDataString str = do
  state <- St.get
  St.put $ state { stringLocations = (Prelude.length (Compiler.Backend.constantPool state)):(stringLocations state) }
  St.modify $ \state -> state { Compiler.Backend.constantPool = (StringEntry str):(Compiler.Backend.constantPool state) }
  return (Prelude.length (Compiler.Backend.constantPool state) - 1)

addString :: String -> Backend Int
addString str = do
  state <- St.get
  case H.lookup str (stringsToIndex state) of
    Just index -> return index
    Nothing -> do
      St.put $ state { stringLocations = (Prelude.length (Compiler.Backend.constantPool state)):(stringLocations state) }
      St.put $ state { stringsToIndex = H.insert str (Prelude.length (Compiler.Backend.constantPool state)) (stringsToIndex state) }
      St.modify $ \state -> state { Compiler.Backend.constantPool = (StringEntry str):(Compiler.Backend.constantPool state) }
      return (Prelude.length (Compiler.Backend.constantPool state) - 1)

addTypeInfo :: TypeInfo -> Backend Int
addTypeInfo info = do
  state <- St.get
  St.put $ state { stringLocations = (Prelude.length (Compiler.Backend.constantPool state)):(stringLocations state) }
  St.modify $ \state -> state { Compiler.Backend.constantPool = (TypeInfoEntry info):(Compiler.Backend.constantPool state) }
  return (Prelude.length (Compiler.Backend.constantPool state) - 1)

compile :: V.Vector SSAFile.File -> V.Vector (String, ClassFile)
compile files = V.map (\file -> compileFile file) files



compileFile :: SSAFile.File -> (String, ClassFile)
compileFile file = evalState (compileFile' file) (BackendState [] 0 0 [] [] [] H.empty)

compileFile' :: SSAFile.File -> Backend (String, ClassFile)
compileFile' SSAFile.File{ SSAFile.packageDec=(AstFile.PackageDec (AstShared.Path packagePath)), SSAFile.imports=imports, SSAFile.primaryClass=primaryClass } = do
  let imports' = Prelude.map (\(AstFile.ImportDec (AstShared.Path path)) -> intercalate "/" path) imports
  let className = SSAClass.className primaryClass
  let classNameStr = intercalate "/" (packagePath Prelude.++ [className])
  classNameStrLocation <- addDataString classNameStr
  _ <- setThisClass $ ClassInfo classNameStrLocation
  _ <- case SSAClass.superClass primaryClass of
    Just (AstClass.SuperClass (AstShared.Path superName) _) -> do
      let superNameStr = intercalate "/" superName
      let superClassOptions = Prelude.filter (\path -> superNameStr `isSuffixOf` path ) imports'
      case superClassOptions of
        [] -> error $ "Super class not found: " Prelude.++ superNameStr
        [superClass] -> do
          superClassLocation <- addString superClass
          setSuperClass $ ClassInfo superClassLocation
        _ -> error $ "Multiple super classes found: " Prelude.++ superNameStr
    Nothing -> do
      objectStrLocation <- addDataString "cocoa/lang/Object"
      setSuperClass $ ClassInfo objectStrLocation
  _ <- compileClass primaryClass
  classFile <- buildClassFile classNameStr
  return (classNameStr, classFile)

compileClass :: SSAClass.Class -> Backend ()
compileClass SSAClass.Class{ SSAClass.visibility=visibility, SSAClass.classType=classType, SSAClass.classTypeParams=classTypeParams, SSAClass.superClass=superClass, SSAClass.interfaces=interfaces, SSAClass.members=members } = do
  let classTypeFlag = case classType of
        AstClass.RegularType -> 0
        AstClass.InterfaceType -> 0x02
        AstClass.AbstractType -> 0x08
  let visibilityFlag = case visibility of
        AstShared.PublicVis -> 0x01
        AstShared.ProtectedVis -> 0x04
        AstShared.PrivateVis -> 0x02
  let classFlags = classTypeFlag .|. visibilityFlag
  _ <- setClassFlags classFlags
  (fieldMembers, otherMembers) <- return $ filterSplit (\m -> case m of
    SSAClass.FieldMember _ -> True
    _ -> False) members
  (methodMembers, classMembers) <- return $ filterSplit (\m -> case m of
    SSAClass.MethodMember _ -> True
    _ -> False) otherMembers
  _ <- Prelude.mapM compileField fieldMembers
  _ <- Prelude.mapM loadMethod methodMembers
  _ <- Prelude.mapM compileMethod methodMembers
  return ()
  
  
  
compileField :: SSAClass.Member -> Backend ()
compileField (SSAClass.FieldMember field) = do
  let AstClass.Field{ AstClass.fieldVisibility=fieldVisibility, AstClass.fieldName=fieldName, AstClass.fieldType=fieldType } = field
  let visibilityFlag = case fieldVisibility of
        AstShared.PublicVis -> 0x01
        AstShared.ProtectedVis -> 0x04
        AstShared.PrivateVis -> 0x02
  let fieldFlags = visibilityFlag
  className <- getCurrentClassName
  let fieldNameStr = className Prelude.++ "/" Prelude.++ fieldName
  fieldNameStrLocation <- addDataString fieldNameStr
  typeInfoLocation <- addTypeInfo $ generateTypeInfo fieldType
  _ <- addField $ FieldInfo fieldNameStrLocation fieldFlags typeInfoLocation
  return ()

generateTypeInfo :: AstShared.Type -> Backend Int
generateTypeInfo info = do
  let typeInfo = matchInfo info
  addTypeInfo typeInfo
  where
    matchInfo :: AstShared.Type -> TypeInfo
    matchInfo (AstShared.Primitive AstShared.U8PrimType) = U8Type
    matchInfo (AstShared.Primitive AstShared.U16PrimType) = U16Type
    matchInfo (AstShared.Primitive AstShared.U32PrimType) = U32Type
    matchInfo (AstShared.Primitive AstShared.U64PrimType) = U64Type
    matchInfo (AstShared.Primitive AstShared.I8PrimType) = I8Type
    matchInfo (AstShared.Primitive AstShared.I16PrimType) = I16Type
    matchInfo (AstShared.Primitive AstShared.I32PrimType) = I32Type
    matchInfo (AstShared.Primitive AstShared.I64PrimType) = I64Type
    matchInfo (AstShared.Primitive AstShared.F32PrimType) = F32Type
    matchInfo (AstShared.Primitive AstShared.F64PrimType) = F64Type
    matchInfo (AstShared.Primitive AstShared.BoolPrimType) = BoolType
    matchInfo (AstShared.Primitive AstShared.CharPrimType) = CharType
    matchInfo (AstShared.Primitive AstShared.UnitPrimType) = UnitType
    matchInfo (AstShared.Array t) = ArrayType $ matchInfo t
    matchInfo (AstShared.ClassType path) = error "TODO: add class info for type"
    matchInfo (AstShared.Arguments t ts) = error "TODO: add arguments info for type"
    matchInfo (AstShared.GenericType t ts) = error "TODO: add generic info for type"
    matchInfo (AstShared.MethodType ts ts' t) = error "TODO: add method info for type"

loadMethod :: SSAClass.Member -> Backend ()
loadMethod (SSAClass.MethodMember method) = do
  let SSAMethod.Method{ SSAMethod.methodVisibility=methodVisibility, SSAMethod.isStatic=isStatic, SSAMethod.isAbstract=isAbstract, SSAMethod.isConst=isConst, SSAMethod.methodName=methodName, SSAMethod.params=params, SSAMethod.returnType=returnType } = method
  className <- getCurrentClassName
  let methodNameStr = className Prelude.++ "/" Prelude.++ methodName
  methodNameStrLocation <- addDataString methodNameStr
  typeInfoLocation <- addTypeInfo $ generateMethodTypeInfo params returnType
  let visibilityFlag = case methodVisibility of
        AstShared.PublicVis -> 0x01
        AstShared.ProtectedVis -> 0x04
        AstShared.PrivateVis -> 0x02
  let staticFlag = if isStatic then 0x08 else 0
  let abstractFlag = if isAbstract then 0x20 else 0
  let constFlag = if isConst then 0x10 else 0
  let methodFlags = visibilityFlag .|. staticFlag .|. abstractFlag .|. constFlag
  methodIndex <- addMethod methodNameStr $ DummyMethodEntry
  addMethodInfo $ MethodInfo methodNameStrLocation methodFlags typeInfoLocation methodIndex

generateMethodTypeInfo :: [AstMethod.Param] -> AstShared.Type -> Backend Int
generateMethodTypeInfo params returnType = do
  argTypeInfo <- Prelude.sequence $ Prelude.map (\(AstMethod.Param _ t) -> generateTypeInfo t) params
  returnTypeInfo <- generateTypeInfo returnType
  addTypeInfo $ Compiler.Backend.ClassFile.MethodType argTypeInfo returnTypeInfo
  
compileMethod :: SSAClass.Member -> Backend ()
compileMethod (SSAClass.MethodMember method) = do
  let SSAMethod.Method{ SSAMethod.isStatic=isStatic, SSAMethod.isAbstract=isAbstract, SSAMethod.isConst=isConst, SSAMethod.methodName=methodName, SSAMethod.params=params, SSAMethod.returnType=returnType, SSAMethod.body=body } = method
  className <- getCurrentClassName
  let methodNameStr = className Prelude.++ "/" Prelude.++ methodName
  methodIndex <- getMethodIndex methodNameStr
  _ <- case body of
    SSAMethod.Prototype -> return ()
    SSAMethod.Native index -> do
      updateMethod methodIndex $ NativeMethodEntry $ read index
      return ()
    SSAMethod.Redirect name -> error "TODO: implement redirect method"
    SSAMethod.MethodBody stmts -> do
      bytecode <- compileMethodBody stmts
      updateMethod methodIndex $ BytecodeMethod bytecode
      return ()
  return ()
  

filterSplit :: (a -> Bool) -> [a] -> ([a], [a])
filterSplit f xs = (Prelude.filter f xs, Prelude.filter (not . f) xs)
