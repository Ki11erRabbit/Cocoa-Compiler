module Main where

import Compiler as C
import Compiler.FileGraph
import System.Environment


main :: IO ()
main = do
  args <- getArgs
  case args of
    dir:_ -> do
      results <- C.readDirectory dir
      case results of
        Left msg -> print msg
        Right xs -> let (names, files) = unzip xs in
          let (vec, nodes, fullNames) = loadNodes names files in do
            print $ fullNames
            print $ nodes
            let edges = createEdges vec nodes in
              let graph = genGraph fullNames edges in do
                print $ topologicalSort graph
    _ -> putStrLn "Please provide a directory to read from"
    
          
  
