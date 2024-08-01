module Compiler.Backend where

import Compiler.SSA.File as SSA
import Compiler.Backend.Bytecode
import Compiler.Backend.ClassFile
import Control.Monad.State

import Data.Vector as V


compile :: V.Vector SSA.File -> V.Vector (String, ClassFile)
compile files = V.map (\file -> compileFile file) files



compileFile :: SSA.File -> (String, ClassFile)
