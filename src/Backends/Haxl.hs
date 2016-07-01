module Backends.Haxl where

import LevelGraphs

import qualified Data.Graph.Inductive as Graph
import Backends.Haskell

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple


-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

cgNodesToHaxlApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToHaxlApplicative graph [] = ""
cgNodesToHaxlApplicative graph [node] = cgNodeToHaskellFunction (Graph.suc graph $ fst node) node
cgNodesToHaxlApplicative graph nodes = "(" ++ (flip replicate ',' $ pred $ length nodes) ++ ") <$> "
                                ++  (List.intercalate " <*> " (map (\x -> flip cgNodeToHaskellFunction x $ Graph.suc graph $ fst x) nodes))

-- FIXME: that's Haxl code not Haskell
-- Here: add recursive call to generate functions in Function and Map nodes
toHaskellDoCodeWrapped :: String -> NestedCodeGraph -> String
toHaskellDoCodeWrapped testname (graph, subgraphs) = testname ++ " :: Env u -> IO Int\n" ++
                                                     testname ++ " myEnv =\n" ++
                                                     "    runHaxl myEnv $ do\n" ++
                                                     toHaskellDoCode graph ++ "\n"

-- FIXME: that's Haxl code not Haskell
-- Here: add recursive call to generate functions in Function and Map nodes
toHaskellDoAppCodeWrapped :: String -> NestedCodeGraph -> String
toHaskellDoAppCodeWrapped testname (graph, subgraphs) =
    testname ++ " :: Env u -> IO Int\n" ++
    testname ++ " myEnv =\n" ++ "    runHaxl myEnv $ do\n" ++
    toHaskellDoAppCode graph ++ "\n"

toHaskellDoCode :: CodeGraph -> String
toHaskellDoCode graph = helperToHaskellDoCode nodes ++ "\n"
    where
      nodes = reverse $ cGraphTopSort graph --bottom up
      trSpace = "  "
      helperToHaskellDoCode ns = (concatMap (\x -> trSpace ++ cgNodeToHaskellDoBind graph x ++ "\n") ns) ++ trSpace ++ "return " ++ nodeToUniqueName (fst $ last ns) ++ "\n"

-- implement haskell subgraphs

-- FIXME: that's Haxl code not Haskell
toHaskellDoAppCode :: CodeGraph -> String
toHaskellDoAppCode graph = helperToDoApp nodes ++ "\n"
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToDoApp [levelNode] = (nodeToUniqueName . fst) levelNode
                                ++ " <- " ++ cgNodesToHaxlApplicative graph [levelNode]
      levelToDoApp levelNodes = "(" ++ List.intercalate ", " (map (nodeToUniqueName . fst) levelNodes)
                                ++ ") <- " ++ cgNodesToHaxlApplicative graph levelNodes
      helperToDoApp [] = ""
      helperToDoApp [[lastLvlNode]] = "        " ++ cgNodesToHaxlApplicative graph [lastLvlNode] ++ "\n"
      helperToDoApp (lvl:lvls) = "        " ++ (levelToDoApp lvl) ++ "\n" ++ (helperToDoApp lvls)
