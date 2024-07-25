module OpsSpec (main, spec) where

import Compiler.Parser.Expr.Ops
import Compiler.Parser.Expr.Shared
import Compiler.Parser.Expr.Base (parseBaseExpr)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Parser
import Ast
import Control.Monad.Combinators.Expr

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseOp" $ do
    it "parses a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12" `shouldParse` UnaryOp NotOp (Literal (IntLit "12"))
    it "parses a binary operator" $ do
      parse (parseOp parseBaseExpr) "" "12 + 2" `shouldParse` BinaryOp Add (Literal (IntLit "12")) (Literal (IntLit "2"))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12 + 2" `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (Literal (IntLit "2"))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "12 + !2" `shouldParse` BinaryOp Add (Literal (IntLit "12")) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12 + !2" `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12 + !2" `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12 + !2" `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12 + !2" `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))
    it "parses a binary operator with a unary operator" $ do
      parse (parseOp parseBaseExpr) "" "!12 + !2" `shouldParse` BinaryOp Add (UnaryOp NotOp (Literal (IntLit "12"))) (UnaryOp NotOp (Literal (IntLit "2")))

