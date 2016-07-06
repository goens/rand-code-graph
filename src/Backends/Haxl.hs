module Backends.Haxl where

import LevelGraphs

import qualified Data.Graph.Inductive as Graph
import Backends.Haskell

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple

cgNodeToHaxlFunction :: [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaxlFunction = cgNodeToHaskellFunction

-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

cgNodesToHaxlApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToHaxlApplicative graph [] = ""
cgNodesToHaxlApplicative graph [node] = cgNodeToHaxlFunction (Graph.suc graph $ fst node) node
cgNodesToHaxlApplicative graph nodes = "(" ++ (flip replicate ',' $ pred $ length nodes) ++ ") <$> "
                                ++  (List.intercalate " <*> " (map (\x -> flip cgNodeToHaxlFunction x $ Graph.suc graph $ fst x) nodes))

-- Here: add recursive call to generate functions in Function and Map nodes
toHaxlDoCodeWrapped :: String -> NestedCodeGraph -> String
toHaxlDoCodeWrapped testname (graph, subgraphs) = testname ++ " :: Env u -> IO Int\n" ++
                                                     testname ++ " myEnv =\n" ++
                                                     "    runHaxl myEnv $ do\n" ++
                                                     toHaskellDoCode graph ++ "\n"

-- Here: add recursive call to generate functions in Function and Map nodes
toHaxlDoAppCodeWrapped :: String -> NestedCodeGraph -> String
toHaxlDoAppCodeWrapped testname (graph, subgraphs) =
    testname ++ " :: Env u -> IO Int\n" ++
    testname ++ " myEnv =\n" ++ "    runHaxl myEnv $ do\n" ++
    toHaxlDoAppCode graph ++ "\n"

-- implement haskell subgraphs

toHaxlDoAppCode :: CodeGraph -> String
toHaxlDoAppCode graph = helperToDoApp nodes ++ "\n"
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToDoApp [levelNode] = (nodeToUniqueName . fst) levelNode
                                ++ " <- " ++ cgNodesToHaxlApplicative graph [levelNode]
      levelToDoApp levelNodes = "(" ++ List.intercalate ", " (map (nodeToUniqueName . fst) levelNodes)
                                ++ ") <- " ++ cgNodesToHaxlApplicative graph levelNodes
      helperToDoApp [] = ""
      helperToDoApp [[lastLvlNode]] = "        " ++ cgNodesToHaxlApplicative graph [lastLvlNode] ++ "\n"
      helperToDoApp (lvl:lvls) = "        " ++ (levelToDoApp lvl) ++ "\n" ++ (helperToDoApp lvls)
