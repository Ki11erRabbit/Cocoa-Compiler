{-# LANGUAGE OverloadedStrings #-}
module Compiler.Parser.Statement where


import Compiler.Ast.Statement
import Compiler.Parser.Shared
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Control.Monad.Combinators
import Debug.Trace(trace)

parseBlock :: Parser [Statement]
parseBlock = do
  _ <- char '{'
  _ <- skipParser
  stmts <- manyTill  (mylexeme parseStatement) (mylexeme $ char '}')
  return stmts

parseStatement :: Parser Statement
parseStatement = parseIfStatement <|>
  parseWhileStatement <|>
  parseForStatement <|>
  parseReturnStatement <|>
  parseBreakStatement <|>
  parseContinueStatement <|>
  parseLetStatement <|>
  parseAssignmentStatement <|>
  parseExprStatement <|>
  parseHangingExprStatement

parseWhileStatement :: Parser Statement
parseWhileStatement = do
  _ <- string "while"
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  stmts <- parseBlock
  _ <- skipParser
  return $ WhileStmt $ WhileStatement expr stmts

parseForStatement :: Parser Statement
parseForStatement = do
  _ <- string "for"
  _ <- skipParser
  leftExpr <- parseLetVar
  _ <- skipParser
  _ <- string "in"
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  stmts <- parseBlock
  _ <- skipParser
  return $ ForStmt $ ForStatement leftExpr expr stmts

parseReturnStatement :: Parser Statement
parseReturnStatement = do
  _ <- string "return"
  _ <- skipParser
  expr <- parseReturn
  _ <- skipParser
  case expr of
    Nothing -> return $ ReturnStmt $ ReturnUnit
    Just e -> return $ ReturnStmt $ ReturnExpr e
  where
    parseReturn = parseUnitReturn <|> parseExprReturn
    parseUnitReturn = do
      _ <- char ';'
      _ <- skipParser
      return Nothing
    parseExprReturn = do
      expr <- parseExpr
      _ <- skipParser
      _ <- char ';'
      _ <- skipParser
      return $ Just expr

parseBreakStatement :: Parser Statement
parseBreakStatement = do
  _ <- string "break"
  _ <- skipParser
  _ <- char ';'
  _ <- skipParser
  return $ BreakStmt $ BreakStatement

parseContinueStatement :: Parser Statement
parseContinueStatement = do
  _ <- string "continue"
  _ <- skipParser
  _ <- char ';'
  _ <- skipParser
  return $ ContinueStmt $ ContinueStatement

parseLetStatement :: Parser Statement
parseLetStatement = do
  _ <- string "let"
  _ <- skipParser
  varName <- parseLetVar
  _ <- skipParser
  _ <- char '='
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  _ <- char ';'
  _ <- skipParser
  return $ LetStmt $ LetStatement varName expr

parseLetVar :: Parser LetVar
parseLetVar = do
  varName <- myidentifier
  _ <- skipParser
  t <- option Nothing $ do
    _ <- char ':'
    _ <- skipParser
    t <- parseType
    return $ Just t
  _ <- skipParser
  return $ LetVar varName t
    
parseAssignmentStatement :: Parser Statement
parseAssignmentStatement = try $ do
  varName <- parseLeftHandExpr
  _ <- skipParser
  _ <- char '='
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  _ <- char ';'
  _ <- skipParser
  return $ AssignmentStmt varName expr

parseExprStatement :: Parser Statement
parseExprStatement = try $ do
  expr <- parseExpr
  _ <- skipParser
  _ <- char ';'
  _ <- skipParser
  return $ ExprStmt expr

parseHangingExprStatement :: Parser Statement
parseHangingExprStatement = try $ do
  expr <- parseExpr
  _ <- skipParser
  return $ Hanging expr

parseIfStatement :: Parser Statement
parseIfStatement = IfStmt <$> parseIfExpr

parseIfExpr :: Parser IfExpr
parseIfExpr = do
  _ <- string "if"
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  stmts <- parseBlock
  elseBlock <- option Nothing $ do
    _ <- string "else"
    _ <- skipParser
    body <- parseElseBody
    return $ Just $ body
  _ <- skipParser
  return $ IfExpr expr stmts elseBlock
  where
    parseElseBody = eitherP parseBlock parseIfExpr 

parseIfExprExpr :: Parser Expr
parseIfExprExpr = IfExprExpr <$> parseIfExpr

parseLeftHandExpr :: Parser Expr
parseLeftHandExpr = try $ (parseVar <|> parseArrayAccessExpr <|> parseFieldAccessExpr)

parseBlockExpr :: Parser Expr
parseBlockExpr = BlockExpr <$> parseBlock

parseExpr :: Parser Expr
parseExpr = try $ (parseIfExprExpr <|> parseBlockExpr <|> parseLeftRecExpr1 <|> parseLeftRecExpr2 <|>  parseNonLeftRecExpr  <?> "expression")

parseNonLeftRecExpr :: Parser Expr
parseNonLeftRecExpr = try $ (parseBaseExpr <|>
  parseArrayExpr <|>
  parseParenExpr <|>
  parseInstanceofExpr <|> 
  parseCastExpr <|>
  parseNewExpr <?> "Parse Non Left Rec Expr")

parseLeftRecExpr1 :: Parser Expr
parseLeftRecExpr1 = try $ ((parseOp parseLeftRecExpr2))


parseLeftRecExpr2 :: Parser Expr
parseLeftRecExpr2 = try $ (parseCallExpr <|>
  parseArrayAccessExpr <|>
  parseFieldAccessExpr <|>
  parseNonLeftRecExpr <?> "Parse Left Rec Expr")

parseNewExpr :: Parser Expr
parseNewExpr = try $ do
  _ <- string "new"
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  args <- emptyArgs <|> nonEmptyArgs
  _ <- skipParser
  return $ NewExpr t args
  where
    emptyArgs = try $ do
      _ <- char '('
      _ <- skipParser
      _ <- char ')'
      _ <- skipParser
      return []
    nonEmptyArgs = do
      _ <- char '('
      _ <- skipParser
      args <- sepBy parseExpr (mylexeme $ char ',')
      _ <- char ')'
      _ <- skipParser
      return args
      


parseArrayExpr :: Parser Expr
parseArrayExpr = do
  exprs <- emptyArray <|> nonEmptyArray
  _ <- skipParser
  return $ ArrayLiteral exprs
  where
    emptyArray = try $ do
      _ <- char '['
      _ <- skipParser
      _ <- char ']'
      return []
    nonEmptyArray = do
      _ <- char '['
      _ <- skipParser
      exprs <- sepBy parseExpr (mylexeme $ char ',')
      _ <- skipParser
      _ <- char ']'
      return exprs
      

parseArrayAccessExpr :: Parser Expr
parseArrayAccessExpr = try $  do
  expr <- parseSimple
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
    parseSimple = parseFieldAccessExpr <|> parseCallExpr <|> parseNonLeftRecExpr

parseFieldAccessExpr :: Parser Expr
parseFieldAccessExpr = try $ do
  expr <- parseSimple
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
    parseSimple = parseNonLeftRecExpr <|> parseArrayAccessExpr <|> parseCallExpr

parseParenExpr :: Parser Expr
parseParenExpr = try $ do
  _ <- char '('
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  _ <- char ')'
  _ <- skipParser
  return $ Paren expr


parseCallExpr :: Parser Expr
parseCallExpr = try $ do
  expr <- parseSimple
  _ <- skipParser
  args <- many calls
  _ <- skipParser
  foldl Call <$> return expr <*> return args
  where
    calls = do
      args <- emptyArgs <|> nonEmptyArgs
      _ <- skipParser
      return args
    parseSimple = parseArrayAccessExpr <|> parseFieldAccessExpr <|> parseNonLeftRecExpr
    emptyArgs = try $ do
      _ <- char '('
      _ <- skipParser
      _ <- char ')'
      _ <- skipParser
      return []
    nonEmptyArgs = do
      _ <- char '('
      _ <- skipParser
      args <- sepBy parseExpr (mylexeme $ char ',')
      _ <- char ')'
      _ <- skipParser
      return args

parseCastExpr :: Parser Expr
parseCastExpr = do
  t <- parseType
  _ <- skipParser
  _ <- char '('
  _ <- skipParser
  expr <- parseExpr
  _ <- skipParser
  _ <- char ')'
  _ <- skipParser
  return $ Cast t expr

parseInstanceofExpr :: Parser Expr
parseInstanceofExpr = do
  _ <- string "instanceof"
  _ <- char '('
  expr <- parseExpr
  _ <- skipParser
  _ <- char ','
  _ <- skipParser
  t <- parseType
  _ <- skipParser
  _ <- char ')'
  _ <- skipParser
  return $ InstanceOf expr t



table = [ [prefix "!" (UnaryOp NotOp), prefix "-" (UnaryOp Neg)]
        , [binary "*" (BinaryOp Mul), binary "/" (BinaryOp Div), binary "%" (BinaryOp Mod)]
        , [binary "+" (BinaryOp Add), binary "-" (BinaryOp Sub)]
        , [binary "<<" (BinaryOp LShift), binary ">>" (BinaryOp RShift)]
        , [binary "<" (BinaryOp Lt), binary ">" (BinaryOp Gt), binary "<=" (BinaryOp Le), binary ">=" (BinaryOp Ge)]
        , [binary "==" (BinaryOp Eq), binary "!=" (BinaryOp Neq)]
        , [binary "&" (BinaryOp BitAnd)]
        , [binary "^" (BinaryOp BitXor)]
        , [binary "|" (BinaryOp BitOr)]
        , [binary "&&" (BinaryOp LogicalAnd)]
        , [binary "||" (BinaryOp LogicalOr)]
        , [binary ".." (BinaryOp ExclusiveRangeOp), binary "..=" (BinaryOp InclusiveRangeOp)]
        ]

binary name f = InfixL (do _ <- string name; _ <- skipParser; return f)
prefix name f = Prefix (do _ <- string name; _ <- skipParser; return f)

parseOp :: Parser Expr -> Parser Expr
parseOp term = do
  makeExprParser (term <|> (parseOp term) <?> "term") table <?> "expression"



 
parseClassLiteralsExpr :: Parser Expr
parseClassLiteralsExpr = parseThisLiteral <|> parseSuperLiteral <|> parseNullLiteral
  where parseThisLiteral = do
          _ <- string "this"
          return ThisExpr
        parseSuperLiteral = do
          _ <- string "super"
          return SuperExpr
        parseNullLiteral = do
          _ <- string "null"
          return NullExpr

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
      charLit <- mychar
      return $ CharLit charLit
    parseStringLiteral = do
      stringLit <- mystring
      return $ StringLit stringLit
    parseBoolLiteral = do
      bool <- mybool
      return $ BoolLit bool


parseVar :: Parser Expr
parseVar = try $ do
  varName <- myidentifier
  return $ Var varName

parseBaseExpr :: Parser Expr
parseBaseExpr = try $ do
  expr <- parseLiteral <|> parseVar <|> parseClassLiteralsExpr <?> "Parse Base Expr"
  return expr
