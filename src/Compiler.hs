{-# LANGUAGE OverloadedStrings #-}
module Compiler where

import Text.Megaparsec
import Text.Megaparsec.Error
import Compiler.FileGraph
import Compiler.Parser.File
import Compiler.Ast.File
import System.Directory
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Either

readDirectory :: FilePath -> IO (Either String [(String, File)])
readDirectory path = do
  files <- listDirectory path
  let fileNames = Prelude.map removeCocoa files in do
    list <- sequence $ (\(fileName, file) ->
      Prelude.readFile (path ++ "/" ++ fileName)) <$> (Prelude.zip files fileNames)
    let results = Prelude.map (\(fileName, file) -> case parse parseFile fileName (pack file) of
                             Left err -> Left $ errorBundlePretty err
                             Right ast -> Right (fileName, ast)) $ Prelude.zip fileNames list in
      case transform results of
        Left xs -> return $ Left $ foldl (\acc err -> acc ++ err) "" xs
        Right xs -> return $ Right xs
    
    
      

transform :: [Either a b] -> Either [a] [b]
transform xs = if (length $ lefts xs) /= 0 then Left $ lefts xs else Right $ rights xs

removeCocoa :: String -> String
removeCocoa (x:'.':'c':'o':'c':'o':'a':_) = [x]
removeCocoa (x:xs) = x:(removeCocoa xs)

