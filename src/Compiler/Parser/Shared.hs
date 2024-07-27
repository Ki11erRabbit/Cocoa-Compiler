{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Shared where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, unpack)
import Data.Void
import Compiler.Ast.Shared
import Control.Monad.Combinators

type Parser = Parsec Void Text

parseTypeParams :: Parser [TypeParam]
parseTypeParams = do
  _ <- char '<'
  _ <- skipParser
  params <- sepBy parseTypeParam (mylexeme $ char ',')
  _ <- skipParser
  _ <- char '>'
  return params

parseTypeParam :: Parser TypeParam
parseTypeParam = generic <|> concrete
  where
    concrete = do
      value <- parseType
      _ <- skipParser
      return $ Concrete value
    generic = try $ do
      name <- parametricTypeIdentifier
      _ <- skipParser
      bounds <- option [] $ do
        _ <- char ':'
        _ <- skipParser
        sepBy parseType (mylexeme $ char '+')
      return $ Generic name bounds

parseVisibility :: Parser Visibility
parseVisibility = do
  visibility <- option "priv" (try (string "pub") <|> (string "priv") <|> (string "prot"))
  _ <- skipParser
  case visibility of
    "pub" -> return PublicVis
    "priv" -> return PrivateVis
    "prot" -> return ProtectedVis

parametricTypeIdentifier :: Parser String
parametricTypeIdentifier = do
  first <- upperChar
  rest <- many alphaNumChar
  _ <- skipParser
  return $ first : rest

parseStatic :: Parser Bool
parseStatic = do
  static <- option "" (string "static")
  _ <- skipParser
  return $ static == "static"

parseAbstract :: Parser Bool
parseAbstract = do
  abstract <- option "" (string "abstract")
  _ <- skipParser
  return $ abstract == "abstract"

parseConst :: Parser Bool
parseConst = do
  const <- option "" (string "const")
  _ <- skipParser
  return $ const == "const"

parsePath :: Parser Path
parsePath = do
  first <- myidentifier
  rest <- many $ do
    _ <- char '.'
    myidentifier
  return $ Path (first : rest)

parseTypePath :: Parser Path
parseTypePath = do
  single <|> multiple
  where
    single = do
      first <- myTypeIdentifier
      return $ Path [first]
    multiple = do
      first <- myidentifier
      rest <- many $ do
        _ <- char '.'
        myidentifier <|> myTypeIdentifier
      return $ Path (first : (rest))


skipParser :: Parser ()
skipParser = space 

myinteger :: Parser String
myinteger = do
  out <- takeWhile1P (Just "integer") (\c -> c >= '0' && c <= '9')
  _ <- skipParser
  return (unpack out)

myfloat :: Parser String
myfloat = try $ do
  whole <- takeWhile1P (Just "whole") (\c -> c >= '0' && c <= '9')
  _ <- char '.'
  decimal <- takeWhile1P (Just "decimal") (\c -> c >= '0' && c <= '9')
  return $ (unpack whole) ++ "." ++ (unpack decimal)

mychar :: Parser Char
mychar = between (char '\'') (char '\'') anyChar

mystring :: Parser String
mystring = do
  _ <- char '"'
  str <- manyTill anyChar (char '"')
  return str

mybool :: Parser Bool
mybool = do
  bool <- try (string "true") <|> string "false"
  return $ (unpack bool) == "true"

anyChar :: Parser Char
anyChar = satisfy (const True)

myidentifier :: Parser String
myidentifier = try $ do
  first <- lowerChar <|> char '_'
  rest <- many alphaNumChar
  _ <- skipParser
  let identifier = first : rest in
    case identifier of
      "true" -> fail "true is a reserved keyword"
      "false" -> fail "false is a reserved keyword"
      "this" -> fail "this is a reserved keyword"
      "super" -> fail "super is a reserved keyword"
      "null" -> fail "null is a reserved keyword"
      "new" -> fail "new is a reserved keyword"
      "instanceof" -> fail "instanceof is a reserved keyword"
      "u8" -> fail "u8 is a reserved keyword"
      "u16" -> fail "u16 is a reserved keyword"
      "u32" -> fail "u32 is a reserved keyword"
      "u64" -> fail "u64 is a reserved keyword"
      "i8" -> fail "i8 is a reserved keyword"
      "i16" -> fail "i16 is a reserved keyword"
      "i32" -> fail "i32 is a reserved keyword"
      "i64" -> fail "i64 is a reserved keyword"
      "f32" -> fail "f32 is a reserved keyword"
      "f64" -> fail "f64 is a reserved keyword"
      "bool" -> fail "bool is a reserved keyword"
      "char" -> fail "char is a reserved keyword"
      "()" -> fail "() is a reserved keyword"
      "pub" -> fail "pub is a reserved keyword"
      "priv" -> fail "priv is a reserved keyword"
      "prot" -> fail "prot is a reserved keyword"
      "static" -> fail "static is a reserved keyword"
      "abstract" -> fail "abstract is a reserved keyword"
      "fn" -> fail "fn is a reserved keyword"
      _ -> return identifier


myTypeIdentifier :: Parser String
myTypeIdentifier = do
  first <- upperChar
  rest <- many alphaNumChar
  _ <- skipParser
  let identifier = first : rest in
      return identifier

mylexeme :: Parser a -> Parser a
mylexeme p = do
  out <- p
  _ <- skipParser
  return out


parseType :: Parser Type
parseType = parsePrimitiveType <|>
  parseArrayType <|>
  parseClassType <|>
  parseArgumentType

parsePrimitiveType :: Parser Type
parsePrimitiveType = try $ do
  type' <- string "u8" <|> string "u16" <|> string "u32" <|> string "u64" <|> string "i8" <|> string "i16" <|> string "i32" <|> string "i64" <|> string "f32" <|> string "f64" <|> string "bool" <|> string "char" <|> string "()" <?> "Primitive Type"
  _ <- skipParser
  case type' of
    "u8" -> return $ Primitive U8PrimType
    "u16" -> return $ Primitive U16PrimType
    "u32" -> return $ Primitive U32PrimType
    "u64" -> return $ Primitive U64PrimType
    "i8" -> return $ Primitive I8PrimType
    "i16" -> return $ Primitive I16PrimType
    "i32" -> return $ Primitive I32PrimType
    "i64" -> return $ Primitive I64PrimType
    "f32" -> return $ Primitive F32PrimType
    "f64" -> return $ Primitive F64PrimType
    "bool" -> return $ Primitive BoolPrimType
    "char" -> return $ Primitive CharPrimType
    "()" -> return $ Primitive UnitPrimType
    _ -> fail "Invalid Primitive Type"

parseArrayType :: Parser Type
parseArrayType = do
  _ <- char '['
  _ <- skipParser
  type' <- parseType
  _ <- skipParser
  _ <- char ']'
  return $ Array type'

parseClassType :: Parser Type
parseClassType = do
  path <- parseTypePath
  _ <- skipParser
  return $ ClassType path


parseArgumentType :: Parser Type
parseArgumentType = do
  type' <- parseType
  _ <- char '<'
  _ <- skipParser
  args <- sepBy parseType (mylexeme $ char ',')
  _ <- skipParser
  _ <- char '>'
  return $ Arguments type' args
