module Main where

import Compiler as C
import Compiler.FileGraph
import System.Environment
import Text.Megaparsec.Error


main :: IO ()
main = do
  args <- getArgs
  case args of
    dir:_ -> do
      results <- C.readDirectory dir
      sequence $ map (\x -> case x of
        Left err -> print err
        Right (name, file) -> print file) results
      return ()
    _ -> putStrLn "Please provide a directory to read from"
    
          
  
