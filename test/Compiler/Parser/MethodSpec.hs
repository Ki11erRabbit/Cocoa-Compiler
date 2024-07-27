{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.MethodSpec (main, spec) where

import Text.Megaparsec
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Compiler.Ast.Method
import Compiler.Parser.Method
import Test.Hspec
import Test.Hspec.Megaparsec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseMethodPrototype" $ do
    it "parses a method prototype" $ do
      parse parseMethodPrototype "" "fn foo() u8;" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive U8PrimType) Prototype
    it "parses a method prototype with no return type" $ do
      parse parseMethodPrototype "" "fn foo();" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive UnitPrimType) Prototype
    it "parses a method prototype with arg" $ do
      parse parseMethodPrototype "" "fn foo(x: u8) u8;" `shouldParse` Method PrivateVis False False False "foo" [] [(Param "x" (Primitive U8PrimType))] (Primitive U8PrimType) Prototype
    it "parses a method prototype with type param" $ do
      parse parseMethodPrototype "" "fn foo<T>() u8;" `shouldParse` Method PrivateVis False False False "foo" [(TypeParam "T" [])] [] (Primitive U8PrimType) Prototype
    it "parses a const method prototype" $ do
      parse parseMethodPrototype "" "const fn foo() u8;" `shouldParse` Method PrivateVis False False True "foo" [] [] (Primitive U8PrimType) Prototype
    it "parses a static method prototype" $ do
      parse parseMethodPrototype "" "static fn foo() u8;" `shouldParse` Method PrivateVis True False False "foo" [] [] (Primitive U8PrimType) Prototype
    it "parses an abstract method prototype" $ do
      parse parseMethodPrototype "" "abstract fn foo() u8;" `shouldParse` Method PrivateVis False True False "foo" [] [] (Primitive U8PrimType) Prototype
    it "parses a public method prototype" $ do
      parse parseMethodPrototype "" "pub fn foo() u8;" `shouldParse` Method PublicVis False False False "foo" [] [] (Primitive U8PrimType) Prototype
    it "parses a protected method prototype" $ do
      parse parseMethodPrototype "" "prot fn foo() u8;" `shouldParse` Method ProtectedVis False False False "foo" [] [] (Primitive U8PrimType) Prototype
  describe "parseNativeMethod" $ do
    it "parses a native method" $ do
      parse parseNativeMethod "" "fn foo() u8 @ 0;" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive U8PrimType) (Native "0")
    it "parses a native method with no return type" $ do
      parse parseNativeMethod "" "fn foo() @ 0;" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive UnitPrimType) (Native "0")
  describe "parseRedirectMethod" $ do
    it "parses a native method" $ do
      parse parseRedirectMethod "" "fn foo() u8 @ bar;" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive U8PrimType) (Redirect "bar")
    it "parses a native method with no return type" $ do
      parse parseRedirectMethod "" "fn foo() @ bar;" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive UnitPrimType) (Redirect "bar")
  describe "parseMethodProper" $ do
    it "parses a method proper" $ do
      parse parseMethodProper "" "fn foo() u8 { return 0; }" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
    it "parses a method proper without return type" $ do
      parse parseMethodProper "" "fn foo() { return 0; }" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive UnitPrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
  describe "parseMethod" $ do
    it "parses a method" $ do
      parse parseMethod "" "fn foo() u8 { return 0; }" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
    it "parses a method with no return type" $ do
      parse parseMethod "" "fn foo() { return 0; }" `shouldParse` Method PrivateVis False False False "foo" [] [] (Primitive UnitPrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
    it "parses a method with arg" $ do
      parse parseMethod "" "fn foo(x: u8) u8 { return x; }" `shouldParse` Method PrivateVis False False False "foo" [] [(Param "x" (Primitive U8PrimType))] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Var "x"))])
    it "parses a method with type param" $ do
      parse parseMethod "" "fn foo<T>() u8 { return 0; }" `shouldParse` Method PrivateVis False False False "foo" [(TypeParam "T" [])] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
    it "parses a const method" $ do
      parse parseMethod "" "const fn foo() u8 { return 0; }" `shouldParse` Method PrivateVis False False True "foo" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
    it "parses a static method" $ do
      parse parseMethod "" "static fn foo() u8 { return 0; }" `shouldParse` Method PrivateVis True False False "foo" [] [] (Primitive U8PrimType) (MethodBody [ReturnStmt (ReturnExpr (Literal (IntLit "0")))])
    it "parses an abstract method" $ do
      parse parseMethod "" "abstract fn foo() u8;" `shouldParse` Method PrivateVis False True False "foo" [] [] (Primitive U8PrimType) Prototype
    
