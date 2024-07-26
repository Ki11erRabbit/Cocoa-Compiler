{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Expr.Compound where

import Compiler.Ast.Expr
import Compiler.Parser.Shared
import Text.Megaparsec
import Text.Megaparsec.Char



parseNewExpr :: Parser Expr
parseNewExpr = do
  _ <- string "new"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  return $ New t


parseArrayExpr :: Parser Expr
parseArrayExpr = do
  _ <- char '['
  _ <- skipParser
  exprs <- sepBy parseExpr (mylexeme $ char ',')
  _ <- skipParser
  _ <- char ']'
  _ <- skipParser
  return $ ArrayLiteral exprs

parseArrayAccessExpr :: Parser Expr
parseArrayAccessExpr = do
  expr <- parseExpr
  _ <- skipParser
  brackedExprs <- many bracketedExpr
  _ <- skipParser
  foldl ArrayAccess <$> return expr <*> return brackedExprs
  where
    bracketedExpr = do
      _ <- char '['
      _ <- skipParser
      e <- parseExpr
      _ <- skipParser
      _ <- char ']'
      _ <- skipParser
      return e

parseFieldAccessExpr :: Parser Expr -> Parser Expr
parseFieldAccessExpr simpleExpr = do
  expr <- simpleExpr
  _ <- skipParser
  accessors <- many fieldAccessor
  _ <- skipParser
  return $ foldl FieldAccess expr accessors
  where
    fieldAccessor = do
      _ <- char '.'
      name <- myidentifier
      _ <- skipParser
      return name

parseParenExpr :: Parser Expr
parseParenExpr = do
  _ <- char '('
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  _ <- char ')'
  _ <- skipParser
  return Paren expr


parseCallExpr :: Parser Expr -> Parser Expr
parseCallExpr simpleExpr = do
  expr <- simpleExpr
  _ <- skipParser
  args <- many calls
  _ <- skipParser
  return $ Call expr args
  where
    calls = do
      _ <- char '('
      _ <- skipParser
      args <- sepBy arg (mylexeme $ char ',')
      _ <- skipParser
      _ <- char ')'
      _ <- skipParser
      return args
    arg = do
      exprs <- sepBy parseExpr (mylexeme $ char ',')
      return exprs

parseCastExpr :: Parser Expr -> Parser Expr
parseCastExpr simpleExpr = do
  expr <- simpleExpr
  _ <- skipParser
  _ <- string "as"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  return $ Cast t expr

parseInstanceofExpr :: Parser Expr -> Parser Expr
parseInstanceofExpr simpleExpr = do
  expr <- simpleExpr
  _ <- skipParser
  _ <- string "instanceof"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  return $ Instanceof expr t

parseCompoundExpr :: Parser Expr -> Parser Expr -> Parser Expr
parseCompoundExpr literals operators = operators (parseCompoundExpr literals operators) <|>
  literals <|>
  parseParenExpr <|>
