{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.StatementSpec (main, spec) where

import Text.Megaparsec
import Compiler.Ast.Shared
import Compiler.Ast.Statement
import Compiler.Parser.Statement
import Test.Hspec
import Test.Hspec.Megaparsec
import Data.Text (pack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseBaseExpr" $ do
    it "parses an int literal" $ do
      parse parseBaseExpr "" "12" `shouldParse` Literal (IntLit "12")
    it "parses a float literal" $ do
      parse parseBaseExpr "" "12.0" `shouldParse` Literal (FloatLit "12.0")
    it "parses a char literal" $ do
      parse parseBaseExpr "" "'a'" `shouldParse` Literal (CharLit 'a')
    it "parses a string literal" $ do
      parse parseBaseExpr "" "\"hello\"" `shouldParse` Literal (StringLit "hello")
    it "parses a boolean literal" $ do
      parse parseBaseExpr "" "true" `shouldParse` Literal (BoolLit True)
    it "parses a variable" $ do
      parse parseBaseExpr "" "x" `shouldParse` Var "x"
    it "parses a this expr" $ do
      parse parseBaseExpr "" "this" `shouldParse` ThisExpr
    it "parses a super expr" $ do
      parse parseBaseExpr "" "super" `shouldParse` SuperExpr
    it "parses a null expr" $ do
      parse parseBaseExpr "" "null" `shouldParse` NullExpr
  describe "parseOp" $ do
    it "parses a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12") `shouldParse` UnaryOp NotOp (Literal (IntLit "12"))
    it "parses a binary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "12 + 2") `shouldParse` BinaryOp Add (Literal (IntLit "12")) (Literal (IntLit "2"))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12 + 2") `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (Literal (IntLit "2"))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "12 + !2") `shouldParse` BinaryOp Add (Literal (IntLit "12")) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12 + !2") `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12 + !2") `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12 + !2") `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12 + !2") `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" (pack "!12 + !2") `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
  describe "parseNewExpr" $ do
    it "parses a new expr" $ do
      parse parseNewExpr "" "new A()" `shouldParse` NewExpr (ClassType (Path ["A"])) []
    it "parses a new expr with args" $ do
      parse parseNewExpr "" "new A(1, 2)" `shouldParse` NewExpr (ClassType (Path ["A"])) [Literal (IntLit "1"), Literal (IntLit "2")]
  describe "parseCallExpr" $ do
    it "parses a call" $ do
      parse parseCallExpr "" "a()" `shouldParse` Call (Var "a") []
    it "parses a call with args" $ do
      parse parseCallExpr "" "a(1, 2)" `shouldParse` Call (Var "a") [Literal (IntLit "1"), Literal (IntLit "2")]
  describe "parseFieldAccessExpr" $ do
    it "parses a field access" $ do
      parse parseFieldAccessExpr "" "a.b" `shouldParse` FieldAccess (Var "a") "b"
  describe "parseArrayAccessExpr" $ do
    it "parses an array access" $ do
      parse parseArrayAccessExpr "" "a[1]" `shouldParse` ArrayAccess (Var "a") (Literal (IntLit "1"))
  describe "parseArrayExpr" $ do
    it "parses an array literal" $ do
      parse parseArrayExpr "" "[1, 2]" `shouldParse` ArrayLiteral [Literal (IntLit "1"), Literal (IntLit "2")]
  describe "parseParenExpr" $ do
    it "parses a paren expr" $ do
      parse parseParenExpr "" "(1)" `shouldParse` Paren (Literal (IntLit "1"))
  describe "parseInstanceofExpr" $ do
    it "parses an instance of expr" $ do
      parse parseInstanceofExpr "" "instanceof(a, A)" `shouldParse` InstanceOf (Var "a") (ClassType (Path ["A"]))
  describe "parseCastExpr" $ do
    it "parses a cast expr" $ do
      parse parseCastExpr "" "A(a)" `shouldParse` Cast (ClassType (Path ["A"])) (Var "a")
  describe "Mixed Expressions" $ do
    it "parses a method call" $ do
      parse parseExpr "" "x.call()" `shouldParse` Call (FieldAccess (Var "x") "call") [] 
    it "parses a method call with arguments" $ do
      parse parseExpr "" "x.call(1,2)" `shouldParse` Call (FieldAccess (Var "x") "call") [Literal (IntLit "1"), (Literal (IntLit "2"))]
    it "parses a field access" $ do
      parse parseExpr "" "x.y" `shouldParse` FieldAccess (Var "x") "y"
    it "parses an array access" $ do
      parse parseExpr "" "x[1]" `shouldParse` ArrayAccess (Var "x") (Literal (IntLit "1"))
    it "parses an array literal" $ do
      parse parseExpr "" "[1, 2]" `shouldParse` ArrayLiteral [Literal (IntLit "1"), Literal (IntLit "2")]
    it "parses a paren expr" $ do
      parse parseExpr "" "(1)" `shouldParse` Paren (Literal (IntLit "1"))
    it "parses an instanceof expr" $ do
      parse parseExpr "" "instanceof(x, A)" `shouldParse` InstanceOf (Var "x") (ClassType (Path ["A"]))
    it "parses a cast expr" $ do
      parse parseExpr "" "A(x)" `shouldParse` Cast (ClassType (Path ["A"])) (Var "x")
    it "parses array concat" $ do
      parse parseExpr "" "[1, 2] + [3, 4]" `shouldParse` BinaryOp Add (ArrayLiteral [Literal (IntLit "1"), Literal (IntLit "2")]) (ArrayLiteral [Literal (IntLit "3"), Literal (IntLit "4")])
    it "parses array concat with a unary operator" $ do
      parse parseExpr "" "!x + [3, 4]" `shouldParse` BinaryOp Add (UnaryOp NotOp (Var "x")) (ArrayLiteral [Literal (IntLit "3"), Literal (IntLit "4")])
    it "parses array concat with a unary operator" $ do
      parse parseExpr "" "[3, 4] + !x" `shouldParse` BinaryOp Add (ArrayLiteral [Literal (IntLit "3"), Literal (IntLit "4")]) (UnaryOp NotOp (Var "x"))
      
  
  
