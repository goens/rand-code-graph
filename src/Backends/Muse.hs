module Backends.Muse where

import LevelGraphs
import Backends.Clojure

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple

cgNodeToMuseFunction  :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToMuseFunction gr children labeledNode@(n,CodeGraphNodeLabel _ ctype t) =
    case ctype of
        -- FIXME
        Map -> "(traverse ifn" ++ nodeToUniqueName n ++ " " ++ childrenStr ++ ")"
        otherwise -> cgNodeToClojureFunction gr children labeledNode
  where
    childrenStr = List.intercalate " " (map nodeToUniqueName children)
    timeout' = fromMaybe n t
    timeoutStr = show timeout'


cgNodeToMuseAppFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToMuseAppFunction graph children labeledNode@(n,CodeGraphNodeLabel _ ctype t) =
    case ctype of
        OtherComputation -> "(<$> compute " ++ wrappedArgumentStr ++ ")"
        -- FIXME
        Map -> "(traverse ifn" ++ nodeToUniqueName n ++ " " ++ childrenStr ++ ")"
        otherwise -> cgNodeToClojureAppFunction graph children labeledNode
  where
    timeout' = fromMaybe n t
    timeoutStr = show timeout'
    childrenStr = List.intercalate " " (map nodeToUniqueName children)
    wrappedArgumentStr = "(return " ++ List.intercalate ") (return " (map nodeToUniqueName children ++ [timeoutStr]) ++ ")"


-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

toMuseMonadCode :: CodeGraph -> String
toMuseMonadCode graph = helperToMuseCode nodes ++ "\n"
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToMuse levelNodes = "mlet [" ++ List.intercalate " " (map (cgNodeToClojureLetDef toMuseMonadCode graph) levelNodes) ++ "]"
      helperToMuseCode [] = ""
      helperToMuseCode [[lastLvlNode]] = cgNodeToMuseFunction graph (Graph.suc graph $ fst lastLvlNode) lastLvlNode ++ "\n"
      helperToMuseCode (lvl:lvls) = "(" ++ (levelToMuse lvl) ++ "\n" ++ (helperToMuseCode lvls) ++ ")"


cgNodesToMuseApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToMuseApplicative graph [] = ""
cgNodesToMuseApplicative graph [node@(nd, _)] = "" ++ (cgNodeToClojureAppFunction graph (Graph.suc graph $ nd) node) ++ ""
cgNodesToMuseApplicative graph nodes = "(<$> clojure.core/vector "
                                ++  (List.intercalate " " (map (\x -> toFun x $ Graph.suc graph $ fst x) nodes)) ++ ")"
    where
      toFun node = flip (cgNodeToMuseAppFunction graph) node


toMuseMonadCodeWrapped :: String -> NestedCodeGraph -> String
toMuseMonadCodeWrapped testname (graph, subgraphs) = (toClojureFunctions toMuseMonadCode subgraphs)
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
        subs = toClojureFunctions toMuseAppCode subgraphs
    in subs ++ "\n" ++ maingraph