{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Method where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Ast.Method
import Compiler.Ast.Shared
import Compiler.Parser.Shared
import Compiler.Parser.Statement

parseRedirectMethod :: Parser Method
parseRedirectMethod = do
  visibility <- parseVisibility
  static <- parseStatic
  abstract <- parseAbstract
  const <- parseConst
  _ <- string "fn"
  _ <- skipParser
  name <- myidentifier
  typeParams <- option [] parseTypeParams
  _ <- skipParser
  params <- parseParams
  _ <- skipParser
  returnType <- parseReturnType
  redirect <- myidentifier
  _ <- skipParser
  _ <- char ';'
  return $ Method visibility static abstract const name typeParams params returnType $ Redirect redirect
  where
    parseReturnType = parseMissingType <|> parseNonMissingType
    parseMissingType = try $ do
      _ <- char '@'
      _ <- skipParser
      return $ Primitive UnitPrimType
    parseNonMissingType = do
      type' <- parseType
      _ <- char '@'
      _ <- skipParser
      return type'

parseNativeMethod :: Parser Method
parseNativeMethod = do
  visibility <- parseVisibility
  static <- parseStatic
  abstract <- parseAbstract
  const <- parseConst
  _ <- string "fn"
  _ <- skipParser
  name <- myidentifier
  typeParams <- option [] parseTypeParams
  _ <- skipParser
  params <- parseParams
  _ <- skipParser
  returnType <- parseReturnType
  index <- myinteger
  _ <- skipParser
  _ <- char ';'
  return $ Method visibility static abstract const name typeParams params returnType $ Native index
  where
    parseReturnType = parseMissingType <|> parseNonMissingType
    parseMissingType = try $ do
      _ <- char '@'
      _ <- skipParser
      return $ Primitive UnitPrimType
    parseNonMissingType = do
      type' <- parseType
      _ <- char '@'
      _ <- skipParser
      return type'

parseMethodPrototype :: Parser Method
parseMethodPrototype = do
  visibility <- parseVisibility
  static <- parseStatic
  abstract <- parseAbstract
  const <- parseConst
  _ <- string "fn"
  _ <- skipParser
  name <- myidentifier
  typeParams <- option [] parseTypeParams
  _ <- skipParser
  params <- parseParams
  _ <- skipParser
  returnType <- parseReturnType
  return $ Method visibility static abstract const name typeParams params returnType Prototype
  where
    parseReturnType = parseMissingType <|> parseNonMissingType
    parseMissingType = try $ do
      _ <- char ';'
      _ <- skipParser
      return $ Primitive UnitPrimType
    parseNonMissingType = do
      type' <- parseType
      _ <- char ';'
      _ <- skipParser
      return type'
      


parseParams :: Parser [Param]
parseParams = do
  params <- emptyParams <|> nonEmptyParams
  return params
  where
    emptyParams = try $ do
      _ <- char '('
      _ <- skipParser
      _ <- char ')'
      _ <- skipParser
      return []
    nonEmptyParams = do
      _ <- char '('
      _ <- skipParser
      args <- sepBy parseParam (mylexeme $ char ',')
      _ <- char ')'
      _ <- skipParser
      return $ args


parseParam :: Parser Param
parseParam = do
  paramName <- myidentifier
  _ <- skipParser
  _ <- char ':'
  _ <- skipParser
  paramType <- parseType
  return $ Param paramName paramType
