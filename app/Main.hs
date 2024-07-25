module Main where

import Parser
import Text.Megaparsec
import Text.Megaparsec.Error
import Data.Text
import Data.Void (Void)


main :: IO ()
main = do
  let input = "!12 + 2 * 4 - 5" in
    case parse parseExpr "" (pack input) of
      Left err -> putStrLn $ errorBundlePretty err
      Right expr -> print expr
