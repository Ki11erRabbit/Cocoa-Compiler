{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.StatementSpec (main, spec) where

import Text.Megaparsec
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
