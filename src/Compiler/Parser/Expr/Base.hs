{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Expr.Base where

import Compiler.Ast.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Parser.Shared


parseLiteral :: Parser Expr
parseLiteral = do
  literal <- parseFloatLiteral <|> parseIntLiteral <|> parseCharLiteral <|> parseStringLiteral <|> parseBoolLiteral
  return $ Literal literal
  where
    parseIntLiteral = do
      int <- myinteger
      return $ IntLit int
    parseFloatLiteral = do
      float <- myfloat
      return $ FloatLit float
    parseCharLiteral = do
      char <- mychar
      return $ CharLit char
    parseStringLiteral = do
      string <- mystring
      return $ StringLit string
    parseBoolLiteral = do
      bool <- mybool
      return $ BoolLit bool


parseVar :: Parser Expr
parseVar = do
  varName <- myidentifier
  return $ Var varName

parseBaseExpr :: Parser Expr
parseBaseExpr = try $ do
  expr <- parseLiteral <|> parseVar
  return expr

myidentifier :: Parser String
myidentifier = do
  first <- letterChar <|> char '_'
  rest <- many alphaNumChar
  _ <- skipParser
  let id = first : rest in
    case id of
      "true" -> fail "true is a reserved keyword"
      "false" -> fail "false is a reserved keyword"
      _ -> return id

