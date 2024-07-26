module Main where

import Compiler.Parser.Statement
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Text
import Data.Void (Void)


main :: IO ()
main = do
  let input = "!12 + 2 * 4 - 5" in
    case parse (parseOp parseBaseExpr) "" (pack input) of
      Left err -> putStrLn $ errorBundlePretty err
      Right expr -> print expr
