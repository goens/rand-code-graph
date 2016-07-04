{-# LANGUAGE TupleSections, BangPatterns #-}
module Backends.Ohua (toOhuaAppCodeWrapped, toOhuaCodeWrapped) where

import Backends.Clojure
import LevelGraphs

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr)
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple
import Control.Monad.State.Strict
import Debug.Trace
import Data.Graph.Inductive.Graph as G


traceWith :: Show a => String -> a -> a
traceWith msg a = trace (msg ++ " " ++ show a) a


assert :: Bool -> a -> a
assert False _ = error "assertion failed"
assert _ a = a

--
-- relabelNodes :: NestedCodeGraph -> NestedCodeGraph
-- relabelNodes (graph, subgraphs) = (graph, ) $ evalState (mapM f subgraphs) (snd $ traceWith "Node range !! " $ Graph.nodeRange graph)
--     where
--         f :: (CodeGraph, String) -> State Int (CodeGraph, String)
--         f (graph, name) = do
--             !s <- get
--             let (_, upper) = Graph.nodeRange graph
--             put $ (s + succ upper)
--             return $ traceWith "after relabel" $ (Graph.gmap (\(a, index, c , d) -> (a, trace "index" $ index + s, c, d)) $ traceWith "Before relabel" graph, name)
--
--
--
-- relabelNodes :: NestedCodeGraph -> NestedCodeGraph
-- relabelNodes (graph, subgraphs) = (graph, ) $ evalState (mapM f subgraphs) (snd $ traceWith "Node range !! " $ Graph.nodeRange graph)
--     where
--         f :: (CodeGraph, String) -> State Int (CodeGraph, String)
--         f (graph, name) = do
--             !s <- get
--             let f2 (_, index, label, _) = insNode (index + s, label)
--                 g = ufold f2 (Graph.empty :: Gr CodeGraphNodeLabel ()) graph
--                 g2 = foldl (\gr (n1, n2) -> insEdge (n1 + s, n2+s, ()) gr) g (edges graph)
--             let (_, upper) = Graph.nodeRange graph
--             put $ (s + succ upper)
--             return (g2, name)
--


-- relabelNodes :: NestedCodeGraph -> NestedCodeGraph
-- relabelNodes (graph, subgraphs) = (graph, ) $ snd $ foldl f ((snd $ Graph.nodeRange graph), []) subgraphs
--     where
--         f :: (Int, [(CodeGraph, String)]) -> (CodeGraph, String) -> (Int, [(CodeGraph, String)])
--         f (s, prev) (graph, name) =
--             let (_, upper) = Graph.nodeRange graph
--                 mapped = traceShowId $ Graph.gmap (\(a, index, c , d) -> (a, index + 100, c, d)) graph
--             in
--                 (s + succ upper, (mapped, name):prev)


cgNodesToOhuaApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToOhuaApplicative graph [] = ""
cgNodesToOhuaApplicative graph [node@(nd, _)] = "" ++ (cgNodeToClojureFunction graph (Graph.suc graph nd) node) ++ ""
cgNodesToOhuaApplicative graph nodes = "(vector " ++ List.intercalate " " (map (\x@(x1,_) -> toFun x $ Graph.suc graph x1) nodes) ++ ")"
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
        subs = toClojureSubFunctions toAppCodeWrapped subgraphs
    in subs ++ "\n" ++ maingraph

-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

toOhuaCode :: CodeGraph -> String
toOhuaCode graph = helperToOhuaCode nodes ++ "\n"
    where
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToOhua levelNodes = "let [" ++ List.intercalate " " (map (cgNodeToClojureLetDef toOhuaCode graph) levelNodes) ++ "]"
      helperToOhuaCode [] = ""
      helperToOhuaCode [[lastLvlNode]] = cgNodeToClojureFunction graph (Graph.suc graph $ fst lastLvlNode) lastLvlNode ++ "\n"
      helperToOhuaCode (lvl:lvls) = "(" ++ (levelToOhua lvl) ++ "\n" ++ (helperToOhuaCode lvls) ++ ")"

toOhuaCodeWrapped :: String -> NestedCodeGraph -> String
toOhuaCodeWrapped testname (graph, subgraphs) =
    toClojureSubFunctions toOhuaCode subgraphs
    ++ "\n" ++ "(defn " ++ testname ++ " []\n(ohua\n" ++ toOhuaCode graph ++ "))"
