module Main where

import Compiler as C
import Compiler.FileGraph
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    dir:_ -> do
      result <- C.readDirectory dir
      case result of
        Left err -> putStrLn $ errorBundlePretty err
        Right (names, files) -> do
          let (vec, nodes, pathChains) = loadNodes names files in
            let graph = genGraph pathChains $ createEdges vec nodes in
              print $ topologicalSort graph
          
  
