module Backends.Muse where

import LevelGraphs
import Backends.Clojure

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple


-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

toMuseMonadCode :: CodeGraph -> String
toMuseMonadCode graph = helperToMuseCode nodes ++ "\n"
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToMuse levelNodes = "mlet [" ++ List.intercalate " " (map (cgNodeToClojureLetDef toMuseMonadCode graph) levelNodes) ++ "]"
      helperToMuseCode [] = ""
      helperToMuseCode [[lastLvlNode]] = cgNodeToClojureFunction graph (Graph.suc graph $ fst lastLvlNode) lastLvlNode ++ "\n"
      helperToMuseCode (lvl:lvls) = "(" ++ (levelToMuse lvl) ++ "\n" ++ (helperToMuseCode lvls) ++ ")"


cgNodesToMuseApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToMuseApplicative graph [] = ""
cgNodesToMuseApplicative graph [node@(nd, _)] = "" ++ (cgNodeToClojureAppFunction graph (Graph.suc graph $ nd) node) ++ ""
cgNodesToMuseApplicative graph nodes = "(<$> clojure.core/vector "
                                ++  (List.intercalate " " (map (\x -> toFun x $ Graph.suc graph $ fst x) nodes)) ++ ")"
    where
      toFun node = flip (cgNodeToClojureAppFunction graph) node


toMuseMonadCodeWrapped :: String -> NestedCodeGraph -> String
toMuseMonadCodeWrapped testname (graph, subgraphs) = (toClojureSubFunctions toMuseMonadCode subgraphs)
                                                     ++ "(defn " ++ testname ++ " []\n(run!! \n" ++ toMuseMonadCode graph ++ "))"

toMuseAppCode :: CodeGraph -> String
toMuseAppCode graph =  helperToMuseApp nodes
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levels = length nodes
      levelToDoApp [levelNode] = (nodeToUniqueName . fst) levelNode ++ " " ++ cgNodesToMuseApplicative graph [levelNode]
      levelToDoApp levelNodes = "[" ++ List.intercalate " " (map (nodeToUniqueName . fst) levelNodes)
                                ++ "] " ++ cgNodesToMuseApplicative graph levelNodes
      helperToMuseApp [] = ""
      helperToMuseApp [[lastnode]] = (if levels == 1 then "" else "]") ++ cgNodeToClojureAppFunction graph [] lastnode
      helperToMuseApp (lvl:lvls) = (levelToDoApp lvl) ++ "\n" ++ (helperToMuseApp lvls) ++ ""

toMuseAppCodeWrapped :: String -> NestedCodeGraph -> String
toMuseAppCodeWrapped testname (graph, subgraphs) =
    let maingraph = if (levelsCGraph graph == 1)
                    then "(defn " ++ testname ++ " [] (run!! \n" ++ toMuseAppCode graph ++ "))\n"
                    else "(defn " ++ testname ++ " [] (run!! \n (mlet [ " ++ toMuseAppCode graph ++ ")))\n"
        subs = toClojureSubFunctions toMuseAppCode subgraphs
    in subs ++ "\n" ++ maingraph