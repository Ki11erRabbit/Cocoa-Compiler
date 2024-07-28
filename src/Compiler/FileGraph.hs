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
createEdges files nodes = Prelude.reverse $ let (_, acc) = V.foldl' (\(i, acc) file -> (i + 1, acc Prelude.++ (createEdge i file nodes))) (0, []) files in acc
  where
    createEdge :: Int -> File -> Nodes -> [EdgeSpec]
    createEdge index file nodes = (\(ImportDec (Path path)) -> EdgeSpec index (nodes H.! path) ) <$> (imports file)

genGraph :: [PathChain] -> [EdgeSpec] -> Gr NodeLabel Int
genGraph names espec = mkGraph nodes edges
  where
    nodes = (\(name, index) -> (index, NodeLabel name)) <$> (Prelude.zip names [0..])
    edges = (\spec -> ((from spec), (to spec), 1)) <$> espec

topologicalSort :: Gr NodeLabel Int -> [Int]
topologicalSort graph = topsort graph
