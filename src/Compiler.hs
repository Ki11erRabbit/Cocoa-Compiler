{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Text.Megaparsec
import Text.Megaparsec.Error
import Compiler.FileGraph
import Compiler.Parser.File
import Compiler.Ast.File
import System.Directory
import Data.Text (pack)

readDirectory :: FilePath -> IO [Either (ParseErrorBundle s e) (String, File)]
readDirectory path = do
  files <- listDirectory path
  let fileNames = Prelude.map removeCocoa files in
    return $ map (\(file, fileName) -> do
      contents <- readFile $ path ++ "/" ++ fileName
      case parse parseFile (pack fileName) (pack contents) of
        Left err -> Left err
        Right file -> Right (file, file)) <$> (Prelude.zip files fileNames)
    
      



removeCocoa :: String -> String
removeCocoa (x:'.':'c':'o':'c':'o':'a':_) = [x]
removeCocoa (x:xs) = x:(removeCocoa xs)

