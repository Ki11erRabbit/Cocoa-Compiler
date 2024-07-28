module Compiler.FileGraph where

import Compiler.Ast.File
import Compiler.Ast.Shared
import Data.HashMap.Strict as H
import Data.Vector as V
import Data.Graph.Inductive.Graph (mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (topsort)
import Data.List as L

type Nodes = H.HashMap [String] Int
type PathChain = [String]

data EdgeSpec = EdgeSpec
  { from :: Int
  , to :: Int
  } deriving (Show, Eq)

newtype NodeLabel = NodeLabel [String] deriving (Show, Eq)


loadNodes :: [String] -> [File] -> (V.Vector File, Nodes, [PathChain])
loadNodes names files = (V.fromList files, H.fromList $ Prelude.zip (Prelude.map (\(name, file) -> let (PackageDec (Path path)) = (packageDec file) in path Prelude.++ [name]) $ (Prelude.zip names files)) [0..], Prelude.map (\(name, file) -> let (PackageDec (Path path)) = (packageDec file) in path Prelude.++ [name]) $ (Prelude.zip names files))


createEdges :: V.Vector File -> Nodes -> [EdgeSpec]
createEdges files nodes = Prelude.reverse $ V.foldl' (\acc file -> acc Prelude.++ (createEdge file nodes)) [] files
  where
    createEdge :: File -> Nodes -> [EdgeSpec]
    createEdge file nodes = Prelude.map (\(ImportDec (Path path)) -> EdgeSpec (nodes H.! path) (nodes H.! (Prelude.init path))) (imports file)

genGraph :: [PathChain] -> [EdgeSpec] -> Gr NodeLabel Int
genGraph names espec = mkGraph nodes edges
  where
    nodes = (\(name, index) -> (index, NodeLabel name)) <$> (Prelude.zip names [0..])
    edges = (\spec -> ((from spec), (to spec), 1)) <$> espec

topologicalSort :: Gr NodeLabel Int -> [Int]
topologicalSort graph = topsort graph
