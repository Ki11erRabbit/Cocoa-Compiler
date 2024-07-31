module Compiler.SSA (convertAstToSsa) where

import Compiler.Ast.File as ASTFile
import Compiler.SSA.File as SSAFile
import Compiler.SSA.Class
import Compiler.SSA.Shared
import Compiler.SSA.Method
import Compiler.SSA.Statement

import Data.Vector as V



convertAstToSsa :: V.Vector ASTFile.File -> V.Vector SSAFile.File
convertAstToSsa = V.map convertFile 

