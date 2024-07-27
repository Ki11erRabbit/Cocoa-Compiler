{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.FileSpec (main, spec) where

import Text.Megaparsec
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Compiler.Ast.Method
import Compiler.Ast.Class
import Compiler.Ast.File
import Compiler.Parser.File
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseFile" $ do
    it "parses a file" $ do
      parse parseFile "" "package foo.bar; import foo.bar.Baz; pub class Foo {}" `shouldParse` File (PackageDec (Path ["foo", "bar"])) [ImportDec (Path ["foo", "bar", "Baz"])] (Class PublicVis "Foo" RegularType [] Nothing [] [])
  describe "parsePackageDec" $ do
    it "parses a package declaration" $ do
      parse parsePackageDec "" "package foo.bar;" `shouldParse` PackageDec (Path ["foo", "bar"])
  describe "parseImportDec" $ do
    it "parses an import declaration" $ do
      parse parseImportDec "" "import foo.bar.Baz;" `shouldParse` ImportDec (Path ["foo", "bar", "Baz"])
