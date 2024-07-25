{-# OverloadedStrings #-}
module Compiler.Parser.Expr.OpsSpec (main, spec) where

import Text.Megaparsec
import Compiler.Ast.Expr
import Compiler.Ast.Shared
import Compiler.Parser.Expr.Ops
import Compiler.Parser.Expr.Shared
import Compiler.Parser.Expr.Base
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Control.Monad.Combinators.Expr
import Data.Text (pack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
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

