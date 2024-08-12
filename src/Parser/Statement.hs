{-# LANGUAGE OverloadedStrings #-}
module Parser.Statement where

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Ast.Statement
import Parser.Shared
import Ast.Shared



parseStatement :: Parser (Spanned Statement)
parseStatement = lexeme $ spanner $ parseExprStmt <|> parseHangingStmt



parseExprStmt :: Parser Statement
parseExprStmt = do
  out <- lexeme $ spanner $ parseExpr
  _ <- char ';'
  return $ ExpressionStmt out

parseHangingStmt :: Parser Statement
parseHangingStmt = do
  out <- lexeme $ spanner $ parseExpr
  return $ HangingStmt out




parseExpr :: Parser Expression
parseExpr = do
  expr <- lexeme $ (parseSimpleExpr <|> parseReturnExpr)
  return expr

parseReturnExpr :: Parser Expression
parseReturnExpr = do
  _ <- lexeme $ string "return"
  body <- optional parseExpr
  return $ ReturnExpr body


parseSimpleExpr :: Parser Expression
parseSimpleExpr = do
  expr <- lexeme $ (parseLiteralExpr <|> (parseOpExpr parseExpr))
  return expr

parseOpExpr :: Parser Expression -> Parser Expression
parseOpExpr p = makeExprParser p opTable
  where
    opTable = [[postfix "?" (PostfixExpr TryOp)]
              ,[prefix "-" (PrefixExpr Neg), prefix "!" (PrefixExpr NotOp)]
              ,[binary "*" (BinaryExpr Mul), binary "/" (BinaryExpr Div), binary "%" (BinaryExpr Mod)]
              ,[binary "+" (BinaryExpr Add), binary "-" (BinaryExpr Sub)]
              ,[binary "<<" (BinaryExpr LShift), binary ">>" (BinaryExpr RShift)]
              ,[binary "&" (BinaryExpr BitAnd)]
              ,[binary "^" (BinaryExpr BitXor)]
              ,[binary "|" (BinaryExpr BitOr)]
              ,[binary "==" (BinaryExpr Eq), binary "!=" (BinaryExpr Neq), binary "<" (BinaryExpr Lt), binary ">" (BinaryExpr Gt), binary "<=" (BinaryExpr Le), binary ">=" (BinaryExpr Ge)]
              ,[binary "&&" (BinaryExpr LogicalAnd)]
              ,[binary "||" (BinaryExpr LogicalOr)]
              ,[binary ".." (BinaryExpr ExclusiveRangeOp), binary "..=" (BinaryExpr InclusiveRangeOp)]]
    binary name f = InfixL (do _ <- string name; _ <- skipParser; return f)
    prefix name f = Prefix (do _ <- string name; _ <- skipParser; return f)
    postfix name f = Postfix (do _ <- string name; _ <- skipParser; return f)
      
    

parseLiteralExpr :: Parser Expression
parseLiteralExpr = do
  out <- parseLiteral
  return out

parseLiteral :: Parser Literal
parseLiteral = do
  out <- parseNumericalLiteral <|> parseStringLiteral <|> parseBooleanLiteral <|> parseCharLiteral
  return out

parseNumericalLiteral :: Parser Literal
parseNumericalLiteral = floatLiteral <|> doubleLiteral <|> decimalLiteral <|> u8Literal <|> u16Literal <|> u32Literal <|> u64Literal <|> i8Literal <|> i16Literal <|> i32Literal <|> i64Literal <|> intLiteral
  where
    floatLiteral :: Parser Literal
    floatLiteral = do
      int <- many digitChar
      _ <- char '.'
      dec <- many digitChar
      _ <- string "f32"
      return $ F32Lit $ (int ++ "." ++ dec)
    doubleLiteral :: Parser Literal
    doubleLiteral = do
      int <- many digitChar
      _ <- char '.'
      dec <- many digitChar
      _ <- string "f64"
      return $ F64Lit $ (int ++ "." ++ dec)
    decimalLiteral :: Parser Literal
    decimalLiteral = do
      int <- many digitChar
      _ <- char '.'
      dec <- many digitChar
      return $ F64Lit $ (int ++ "." ++ dec)
    u8Literal :: Parser Literal
    u8Literal = do
      int <- many digitChar
      _ <- string "u8"
      return $ U8Lit $ int
    u16Literal :: Parser Literal
    u16Literal = do
      int <- many digitChar
      _ <- string "u16"
      return $ U16Lit $ int
    u32Literal :: Parser Literal
    u32Literal = do
      int <- many digitChar
      _ <- string "u32"
      return $ U32Lit $ int
    u64Literal :: Parser Literal
    u64Literal = do
      int <- many digitChar
      _ <- string "u64"
      return $ U64Lit $ read int
    i8Literal :: Parser Literal
    i8Literal = do
      int <- many digitChar
      _ <- string "i8"
      return $ I8Lit $ read int
    i16Literal :: Parser Literal
    i16Literal = do
      int <- many digitChar
      _ <- string "i16"
      return $ I16Lit $ read int
    i32Literal :: Parser Literal
    i32Literal = do
      int <- many digitChar
      _ <- string "i32"
      return $ I32Lit $ read int
    i64Literal :: Parser Literal
    i64Literal = do
      int <- many digitChar
      _ <- string "i64"
      return $ I64Lit $ read int
    intLiteral :: Parser Literal
    intLiteral = do
      int <- many digitChar
      return $ IntLit $ read int
      
      
parseStringLiteral :: Parser Literal
parseStringLiteral = do
  _ <- char '"'
  str <- many $ (noneOf ['"']) <|> do
    _ <- char '\\'
    c <- alphaNumChar
    case c of
      'n' -> return '\n'
      't' -> return '\t'
      'r' -> return '\r'
      '0' -> return '\0'
      _ -> return c
  _ <- char '"'
  return $ StringLit str

parseBooleanLiteral :: Parser Literal
parseBooleanLiteral = do
  bool <- try (string "true") <|> string "false"
  return $ BoolLit $ bool == "true"

parseCharLiteral :: Parser Literal
parseCharLiteral = newlineChar <|> tabChar <|> carriageReturnChar <|> nullChar <|> normalChar
  where
    newlineChar = do
      _ <- string "'\\n'"
      return $ CharLit '\n'
    tabChar = do
      _ <- string "'\\t'"
      return $ CharLit '\t'
    carriageReturnChar = do
      _ <- string "'\\r'"
      return $ CharLit '\r'
    nullChar = do
      _ <- string "'\\0'"
      return $ CharLit '\0'
    normalChar = do
      _ <- char '\''
      c <- alphaNumChar
      _ <- char '\''
      return $ CharLit c
