{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Shared where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, unpack)
import Data.Void


type Parser = Parsec Void Text

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
  char '"'
  str <- manyTill anyChar (char '"')
  return str

mybool :: Parser Bool
mybool = do
  bool <- try (string "true") <|> string "false"
  return $ (unpack bool) == "true"

anyChar :: Parser Char
anyChar = satisfy (const True)

    
