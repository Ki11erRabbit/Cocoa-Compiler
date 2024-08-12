{-# LANGUAGE OverloadedStrings #-}
module Parser.Shared (Parser, spanner, skipParser, lexeme) where

import Ast.Shared
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text)
import Data.Void (Void)

type Parser a = Parsec Void Text a



spanner :: Parser a -> Parser (Spanned a)
spanner p = do
  start <- getSourcePos
  value <- p
  end <- getSourcePos
  return (Spanned (Span start end) value)

lexeme :: Parser a -> Parser a
lexeme p = p <* skipParser

skipParser :: Parser ()
skipParser = space <|> comment

comment :: Parser ()
comment = lineComment <|> blockComment

lineComment :: Parser ()
lineComment = do
  _ <- string "//"
  _ <- manyTill anySingle eol
  return ()

blockComment :: Parser ()
blockComment = do
  _ <- string "/*"
  _ <- manyTill anySingle (string "*/")
  return ()
