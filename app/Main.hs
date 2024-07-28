module Main (main, runTypeChecker) where

import Compiler as C
import Compiler.FileGraph
import System.Environment
import Compiler.TypeChecker as TC

main :: IO ()
main = do
  args <- getArgs
  runTypeChecker args
    
runTypeChecker :: [String] -> IO ()       
runTypeChecker args = do
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
              let graph = genGraph fullNames edges in
                let sorted = reverse $ topologicalSort graph in do
                _ <- print sorted
                case TC.topLevelTypeCheck vec sorted of
                  Left msg -> print msg
                  Right _ -> print "Type checking successful"
    _ -> putStrLn "Please provide a directory to read from" 
