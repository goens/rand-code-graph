{-# LANGUAGE TupleSections #-}
module Backends.Ohua (toOhuaAppCodeWrapped, toOhuaCodeWrapped) where

import           Backends.Clojure
import           LevelGraphs

import           Control.Monad.State.Strict
import           Data.Graph.Inductive       (Gr)
import qualified Data.Graph.Inductive       as Graph
import           Data.Graph.Inductive.Graph as G
import qualified Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Tuple                 as Tuple
import           Debug.Trace

import           Data.Maybe                 (fromMaybe)
import Text.Printf


cgNodeToOhua  :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToOhua gr children labeledNode@(n,label) =
    case computationType label of
        Map -> printf "(count (smap ifn%s (mvector %s)))" (nodeToUniqueName n) childrenStr
        -- HACK (temporary)
        NamedFunction name -> printf "(let [v (count (mvector %s))] (seq v (%s v)))" childrenStr name
        -- HACK (temporary)
        Function -> printf "(let [v (count (mvector %s))] (seq v (ifn%s v)))" childrenStr (nodeToUniqueName n)
        otherwise -> cgNodeToClojureFunction gr children labeledNode
  where
    childrenStr = List.intercalate " " (map nodeToUniqueName children)
    timeout' = fromMaybe n (timeout label)
    timeoutStr = show timeout'

cgNodeToOhuaApp :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToOhuaApp = cgNodeToOhua

toOhuaAlgorithms :: (CodeGraph -> String) -> [FunctionGraph] -> String
toOhuaAlgorithms toCode subGraphs = toClojureSubFunctions toCode subGraphs (toClojureSubFunctionHead "defalgo")

cgNodesToOhuaApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToOhuaApplicative graph [] = ""
cgNodesToOhuaApplicative graph [node@(nd, _)] = "" ++ (cgNodeToClojureFunction graph (Graph.suc graph nd) node) ++ ""
cgNodesToOhuaApplicative graph nodes = "(mvector " ++ List.intercalate " " (map (\x@(x1,_) -> toFun x $ Graph.suc graph x1) nodes) ++ ")"
    where
      toFun node = flip (cgNodeToClojureFunction graph) node

toOhuaAppCode :: CodeGraph -> String
toOhuaAppCode graph = helperToOhuaApp nodes
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levels = length nodes
      levelToDoApp [levelNode] = (nodeToUniqueName . fst) levelNode ++ " " ++ cgNodesToOhuaApplicative graph [levelNode]
      levelToDoApp levelNodes = "[" ++ List.intercalate " " (map (nodeToUniqueName . fst) levelNodes)
                                ++ "] " ++ cgNodesToOhuaApplicative graph levelNodes
      helperToOhuaApp [] = ""
      helperToOhuaApp [[lastnode]] = (if levels == 1 then "" else "]") ++ cgNodeToClojureFunction graph (Graph.suc graph $ fst lastnode) lastnode
      helperToOhuaApp (lvl:lvls) = (levelToDoApp lvl) ++ "\n" ++ (helperToOhuaApp lvls) ++ ""

toOhuaAppCodeWrapped :: String -> NestedCodeGraph -> String
toOhuaAppCodeWrapped testname (graph, subgraphs) =
    let maingraph = if (levelsCGraph graph == 1)
                    then "(defn " ++ testname ++ " [] (ohua \n" ++ toOhuaAppCode graph ++ "))\n"
                    else "(defn " ++ testname ++ " [] (ohua \n (let [ " ++ toOhuaAppCode graph ++ ")))\n"
        toAppCodeWrapped graph =
            if levelsCGraph graph == 1
                then toOhuaAppCode graph
                else "(let [" ++ toOhuaAppCode graph ++ ")"
        subs = toOhuaAlgorithms toAppCodeWrapped subgraphs
    in subs ++ "\n" ++ maingraph

-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)


cgNodeToOhuaLetDef :: CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToOhuaLetDef graph x@(x1,_) = nodeToUniqueName x1 ++ " " ++ cgNodeToOhua graph (Graph.suc graph x1) x


toOhuaCode :: CodeGraph -> String
toOhuaCode graph = helperToOhuaCode nodes ++ "\n"
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToOhua levelNodes = "let [" ++ List.intercalate " " (map (cgNodeToOhuaLetDef graph) levelNodes) ++ "]"
      helperToOhuaCode [] = ""
      helperToOhuaCode [[lastLvlNode]] = cgNodeToOhua graph (Graph.suc graph $ fst lastLvlNode) lastLvlNode ++ "\n"
      helperToOhuaCode (lvl:lvls) = "(" ++ (levelToOhua lvl) ++ "\n" ++ (helperToOhuaCode lvls) ++ ")"

toOhuaCodeWrapped :: String -> NestedCodeGraph -> String
toOhuaCodeWrapped testname (graph, subgraphs) =
    toOhuaAlgorithms toOhuaCode subgraphs
    ++ "\n" ++ "(defn " ++ testname ++ " []\n(ohua\n" ++ toOhuaCode graph ++ "))"
