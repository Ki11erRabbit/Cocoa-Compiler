{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.File where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Ast.File
import Compiler.Ast.Class
import Compiler.Ast.Shared
import Compiler.Parser.Shared
import Compiler.Parser.Class


parseFile :: Parser File
parseFile = do
  _ <- skipParser
  packageDec <- parsePackageDec
  _ <- skipParser
  importDecs <- option [] $ many parseImportDec
  _ <- skipParser
  class' <- parseClass
  return $ File packageDec importDecs class'

parsePackageDec :: Parser PackageDec
parsePackageDec = do
  _ <- string "package"
  _ <- skipParser
  name <- parsePath
  _ <- skipParser
  _ <- char ';'
  return $ PackageDec name

parseImportDec :: Parser ImportDec
parseImportDec = do
  _ <- string "import"
  _ <- skipParser
  name <- parseTypePath
  _ <- skipParser
  _ <- char ';'
  return $ ImportDec name

