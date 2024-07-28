module Compiler.TypeChecker where

import Compiler.Ast.File
import Compiler.Ast.Class
import Compiler.Ast.Method
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Data.HashMap.Strict as H
import Data.HashSet as S
import Data.Vector as V

topLevelTypeCheck :: V.Vector File -> [Int] -> Either String ()
topLevelTypeCheck files names order = checkFiles files order H.empty


checkFiles :: V.Vector File -> [Int] -> H.HashMap [String] [Type] -> Either String ()
checkFiles files (next:rest) types = do
  let file = files V.! next
  newTypes <- checkFile file types
  checkFiles files rest newTypes

checkFile :: File -> H.HashMap [String] [Type] -> Either [String] (H.HashMap [String] [Type])
checkFile File{packageDec=(PackageDec (Path package)), primaryClass=primaryClass, imports=imports} types = do
  let imports = Prelude.map (\path -> path) imports
  typesToAdd <- (getTypes package types imports Nothing primaryClass)
  let newTypes = H.insert (package Prelude.++ [className primaryClass]) typesToAdd types
  checkClass primaryClass newTypes
  
