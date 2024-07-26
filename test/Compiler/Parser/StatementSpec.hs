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
    it "parses block expression" $ do
      parse parseExpr "" "{1; 2}" `shouldParse` BlockExpr [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))]
    it "parses an if expr" $ do
      parse parseExpr "" "if true {1; 2} else {3; 4}" `shouldParse` IfExprExpr (IfExpr (Literal (BoolLit True)) [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))] (Just (Left [ExprStmt (Literal (IntLit "3")), Hanging (Literal (IntLit "4"))])))
  describe "parseLetStatement" $ do
    it "parses a let statement" $ do
      parse parseLetStatement "" "let x = 1;" `shouldParse` LetStmt (LetStatement (LetVar "x") Nothing (Literal (IntLit "1")))
    it "parses a let statement with a type" $ do
      parse parseLetStatement "" "let x: Int = 1;" `shouldParse` LetStmt (LetStatement (LetVar "x") (Just (ClassType (Path ["Int"]))) (Literal (IntLit "1")))
  describe "parseReturnStatement" $ do
    it "parses a return statement" $ do
      parse parseReturnStatement "" "return 1;" `shouldParse` ReturnStmt (ReturnExpr (Literal (IntLit "1")))
    it "parses a return statement with no value" $ do
      parse parseReturnStatement "" "return;" `shouldParse` ReturnStmt ReturnUnit
  describe "parseIf Statement" $ do
    it "parses an if statement" $ do
      parse parseIfStatement "" "if true {1; 2}" `shouldParse` IfStmt (IfExpr (Literal (BoolLit True)) [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))] Nothing)
    it "parses an if statement with an else" $ do
      parse parseIfStatement "" "if true {1; 2} else {3; 4}" `shouldParse` IfStmt (IfExpr (Literal (BoolLit True)) [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))] (Just (Left [ExprStmt (Literal (IntLit "3")), Hanging (Literal (IntLit "4"))])))
  describe "parseWhile Statement" $ do
    it "parses a while statement" $ do
      parse parseWhileStatement "" "while true {1; 2}" `shouldParse` WhileStmt (WhileStatement (Literal (BoolLit True)) [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))])
  describe "parseFor Statement" $ do
    it "parses a for statement" $ do
      parse parseForStatement "" "for x in y {1; 2}" `shouldParse` ForStmt (ForStatement (LetVar "x") (Var "y") [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))])
  describe "parseBreak Statement" $ do
    it "parses a break statement" $ do
      parse parseBreakStatement "" "break;" `shouldParse` BreakStmt BreakStatement
  describe "parseContinue Statement" $ do
    it "parses a continue statement" $ do
      parse parseContinueStatement "" "continue;" `shouldParse` ContinueStmt ContinueStatement
  describe "parseExpr Statement" $ do
    it "parses an expr statement" $ do
      parse parseExprStatement "" "1;" `shouldParse` ExprStmt (Literal (IntLit "1"))
  describe "parseHanging Statement" $ do
    it "parses a hanging statement" $ do
      parse parseHangingExprStatement "" "1" `shouldParse` Hanging (Literal (IntLit "1"))
  describe "parseStatement" $ do
    it "parses a let statement" $ do
      parse parseStatement "" "let x = 1;" `shouldParse` LetStmt (LetStatement (LetVar "x") Nothing (Literal (IntLit "1")))
    it "parses a return statement" $ do
      parse parseStatement "" "return 1;" `shouldParse` ReturnStmt (ReturnExpr (Literal (IntLit "1")))
    it "parses an if statement" $ do
      parse parseStatement "" "if true {1; 2}" `shouldParse` IfStmt (IfExpr (Literal (BoolLit True)) [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))] Nothing)
    it "parses a while statement" $ do
      parse parseStatement "" "while true {1; 2}" `shouldParse` WhileStmt (WhileStatement (Literal (BoolLit True)) [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))])
    it "parses a for statement" $ do
      parse parseStatement "" "for x in y {1; 2}" `shouldParse` ForStmt (ForStatement (LetVar "x") (Var "y") [ExprStmt (Literal (IntLit "1")), Hanging (Literal (IntLit "2"))])
    it "parses a break statement" $ do
      parse parseStatement "" "break;" `shouldParse` BreakStmt BreakStatement
    it "parses a continue statement" $ do
      parse parseStatement "" "continue;" `shouldParse` ContinueStmt ContinueStatement
    it "parses an expr statement" $ do
      parse parseStatement "" "1;" `shouldParse` ExprStmt (Literal (IntLit "1"))
    it "parses a hanging statement" $ do
      parse parseStatement "" "1" `shouldParse` Hanging (Literal (IntLit "1"))
    

  
