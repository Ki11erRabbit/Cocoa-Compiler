{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Class where

import Text.Megaparsec
import Text.Megaparsec.Char
import Compiler.Ast.Method
import Compiler.Ast.Class
import Compiler.Ast.Shared
import Compiler.Parser.Shared
import Compiler.Parser.Method







parseClass :: Parser Class
parseClass = do
  visibility <- parseVisibility
  classType <- parseTypeOfClass
  _ <- skipParser
  name <- myTypeIdentifier
  typeParams <- option [] parseTypeParams
  _ <- skipParser
  super <- parseSuperClass
  _ <- skipParser
  interfaces <- parseImplements
  _ <- char '{'
  _ <- skipParser
  members <- manyTill parseMembers (mylexeme $ char '}')
  return $ Class visibility name classType typeParams super interfaces members


parseTypeOfClass :: Parser ClassType
parseTypeOfClass = interface <|> abstract <|> normal
  where
    interface = do
      _ <- string "interface"
      _ <- skipParser
      return InterfaceType
    abstract = do
      _ <- string "abstract"
      _ <- skipParser
      _ <- string "class"
      _ <- skipParser
      return AbstractType
    normal = do
      _ <- string "class"
      _ <- skipParser
      return RegularType

parseSuperClass :: Parser (Maybe SuperClass)
parseSuperClass = option Nothing $ do
  _ <- string "extends"
  _ <- skipParser
  super <- parseSuper
  return $ Just super
  where
    parseSuper = do
      super <- parseTypePath
      typeParams <- option [] parseTypeParams
      return $ SuperClass super typeParams

parseImplements :: Parser [SuperClass]
parseImplements = option [] $ do
  _ <- string "implements"
  _ <- skipParser
  interfaces <- sepBy parseInterface (mylexeme $ char ',')
  return interfaces
  where
    parseInterface = do
      interface <- parseTypePath
      typeParams <- option [] parseTypeParams
      return $ SuperClass interface typeParams


parseMembers :: Parser Member
parseMembers = parseClassMethod <|> parseClassField <|> parseSubClass

parseClassMethod :: Parser Member
parseClassMethod = do
  method <- parseMethod
  return $ MethodMember method

parseClassField :: Parser Member
parseClassField = do
  visibility <- parseVisibility
  _ <- skipParser
  name <- myidentifier
  _ <- skipParser
  _ <- char ':'
  _ <- skipParser
  fieldType <- parseType
  _ <- skipParser
  _ <- char ';'
  _ <- skipParser
  return $ FieldMember $ Field visibility name fieldType

parseSubClass :: Parser Member
parseSubClass = do
  class' <- parseClass
  return $ ClassMember class'
