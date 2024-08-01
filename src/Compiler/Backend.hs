module Compiler.Backend (compile) where

import Compiler.SSA.File as SSAFile
import Compiler.Ast.File as AstFile
import Compiler.Ast.Shared as AstShared
import Compiler.SSA.Class as SSAClass
import Compiler.Backend.Bytecode
import Compiler.Backend.ClassFile
import Control.Monad.State
import Data.List
import Data.Vector as V
import Data.HashMap.Strict as H

data BackendState = BackendState
  { constantPool :: [PoolEntry]
  , thisClassLocation :: Int
  , superClassLocation :: Int
  , classFlags :: Int8
  , fieldInfo :: [FieldInfo]
  , methodInfo :: [MethodInfo]
  , interfaceInfo :: [InterfaceInfo]
  , stringLocations :: [Int]
  , previousClassDeclarations :: H.HashMap String ClassFile
  , methodNamesToIndex :: H.HashMap String Int
  , stringsToIndex :: H.HashMap String Int
  } deriving (Show, Eq)

type Backend a = State BackendState a

getClassFile :: String -> Backend ClassFile
getClassFile className = do
  state <- get
  case H.lookup className (previousClassDeclarations state) of
    Just classFile -> return classFile
    Nothing -> error $ "Class not found: " ++ className

-- TODO: Write a function that resets the State and turns it into a ClassFile and returns it/puts it in the hashmap
buildClassFile :: String -> Backend ClassFile
buildClassFile className = do
  state <- get
  let classFile = ClassFile (thisClassLocation state) (superClassLocation state) (classFlags state) (Prelude.reverse $ constantPool state) (Prelude.reverse $ fieldInfo state) (Prelude.reverse $ methodInfo state) (Prelude.reverse $ interfaceInfo state) (Prelude.reverse $ stringLocations state)
  put $ BackendState [] 0 0 0 [] [] [] (H.insert className classFile (previousClassDeclarations state))
  return classFile


setThisClass :: ClassInfo -> Backend ()
setThisClass info = do
  state <- get
  put $ state { thisClassLocation = length (constantPool state) }
  modify $ \state -> state { constantPool = (ClassInfoEntry info):(constantPool state) }

setSuperClass :: ClassInfo -> Backend ()
setSuperClass info = do
  state <- get
  put $ state { superClassLocation = length (constantPool state) }
  modify $ \state -> state { constantPool = (ClassInfoEntry info):(constantPool state) }

addField :: FieldInfo -> Backend ()
addField info = do
  state <- get
  modify $ \state -> state { fieldInfo = info:(fieldInfo state) }

addMethod :: String -> Method -> Backend Int
addMethod name method = do
  state <- get
  let index = length (methodInfo state)
  put $ state { methodNamesToIndex = H.insert name index (methodNamesToIndex state) }
  modify $ \state -> state { methodInfo = (MethodEntry name method):(methodInfo state) }
  return index

getMethodIndex :: String -> Backend Int
getMethodIndex name = do
  state <- get
  case H.lookup name (methodNamesToIndex state) of
    Just index -> return index
    Nothing -> error $ "Method not found: " ++ name

addMethodInfo :: MethodInfo -> Backend ()
addMethodInfo info = do
  state <- get
  modify $ \state -> state { methodInfo = info:(methodInfo state) }

addInterfaceInfo :: InterfaceInfo -> Backend ()
addInterfaceInfo info = do
  state <- get
  modify $ \state -> state { interfaceInfo = info:(interfaceInfo state) }

addDataString :: String -> Backend Int
addDataString str = do
  state <- get
  put $ state { stringLocations = (length (constantPool state)):(stringLocations state) }
  modify $ \state -> state { constantPool = (DataStringEntry str):(constantPool state) }
  return (length (constantPool state) - 1)

addString :: String -> Backend Int
addString str = do
  state <- get
  case H.lookup str (stringsToIndex state) of
    Just index -> return index
    Nothing -> do
      put $ state { stringLocations = (length (constantPool state)):(stringLocations state) }
      put $ state { stringsToIndex = H.insert str (length (constantPool state)) (stringsToIndex state) }
      modify $ \state -> state { constantPool = (StringEntry str):(constantPool state) }
      return (length (constantPool state) - 1)

addTypeInfo :: TypeInfo -> Backend Int
addTypeInfo info = do
  state <- get
  put $ state { stringLocations = (length (constantPool state)):(stringLocations state) }
  modify $ \state -> state { constantPool = (TypeInfoEntry info):(constantPool state) }
  return (length (constantPool state) - 1)

compile :: V.Vector SSAFile.File -> V.Vector (String, ClassFile)
compile files = V.map (\file -> compileFile file) files



compileFile :: SSAFile.File -> (String, ClassFile)
compileFile file = evalState (compileFile' file) (BackendState [] 0 0 [] [] [] H.empty)

compileFile' :: SSAFile.File -> Backend (String, ClassFile)
compileFile' SSAFile{ packageDec=(AstFile.PackageDec (AstShared.Path packgePath)), imports=imports, primaryClass=primaryClass } = do
  let imports' = Prelude.map (\(AstFile.ImportDec (AstShared.Path path)) -> intercalate "/" path) imports
  let className = SSAClass.className primaryClass
  let classNameStr = intercalate "/" (packagePath ++ [className])
  classNameStrLocation <- addDataString classNameStr
  _ <- setThisClass $ ClassInfo classNameStrLocation
  _ <- case of SSAClass.superClass primaryClass of
    Just (AstClass.SuperClass (AstShared.Path superName)) -> do
      let superNameStr = intercalate "/" superName
      let superClassOptions = Prelude.filter (\path -> superNameStr `isSuffixOf` path ) imports'
      case superClassOptions of
        [] -> error $ "Super class not found: " ++ superNameStr
        [superClass] -> do
          superClassLocation <- addString superClass
          setSuperClass $ ClassInfo superClassLocation
        _ -> error $ "Multiple super classes found: " ++ superNameStr
    Nothing -> do
      objectStrLocation <- addDataString "cocoa/lang/Object"
      setSuperClass $ ClassInfo objectStrLocation
  

  

