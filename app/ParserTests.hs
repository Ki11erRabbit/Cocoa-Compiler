module ParserTests where

import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Parser
import Ast
import Text.Megaparsec



main :: IO ()
main = hspec $ do
  describe "parseLiteral" $ do
    it "parses an integer literal" $ do
      parse parseLiteral "" "12" `shouldParse` Literal (IntLit 12)
    it "parses a float literal" $ do
      parse parseLiteral "" "12.0" `shouldParse` Literal (FloatLit 12.0)
    it "parses a char literal" $ do
      parse parseLiteral "" "'a'" `shouldParse` Literal (CharLit 'a')
    it "parses a string literal" $ do
      parse parseLiteral "" "\"hello\"" `shouldParse` Literal (StringLit "hello")
    it "parses a boolean literal" $ do
      parse parseLiteral "" "true" `shouldParse` Literal (BoolLit True)

