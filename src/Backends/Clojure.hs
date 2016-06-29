module Backends.Clojure where

import LevelGraphs

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple

cgNodeToClojureFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,DataSource,time)) =
    "(get-data " ++ List.intercalate " " (map nodeToUniqueName children) ++ " \"service-name\" " ++ (show $ fromMaybe n time)  ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,SlowDataSource,time)) =
    "(slow-get-data " ++ List.intercalate " " (map nodeToUniqueName children) ++ " \"service-name\" " ++ (show $ 10000 + fromMaybe n time)  ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,OtherComputation,time)) =
    "(compute " ++ List.intercalate " " (map nodeToUniqueName children) ++ " " ++ (show $ fromMaybe n time) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,SideEffect,time)) =
    "(write-data " ++ List.intercalate " " (map nodeToUniqueName children) ++ " \"service-name\" " ++ (show $ fromMaybe n time) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,NamedFunction name,time)) =
    "(" ++ name ++ (if null children then [] else " ") ++ List.intercalate " " (map nodeToUniqueName children) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,Function,time)) =
    "(ifn" ++ nodeToUniqueName n ++ (if null children then [] else " ") ++ List.intercalate " " (map nodeToUniqueName children) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,Map,time)) =
    "(map ifn" ++ nodeToUniqueName n ++ " [" ++ List.intercalate " " (map nodeToUniqueName children) ++ "] " ++ ")"
cgNodeToClojureFunction graph _ (_,CodeGraphNodeLabel (_,Conditional (CondBranch cond) trueBranch falseBranch,_)) =
    "(if " ++ nodeToUniqueName cond ++ " " ++ List.intercalate " " (map maybeNodeToUniqueName [trueBranch,falseBranch] ) ++ ")"
    where maybeNodeToUniqueName CondNil = "nil"
          maybeNodeToUniqueName (CondBranch node) = nodeToUniqueName node

cgNodeToClojureAppFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,DataSource,time)) =
    "(get-data " ++ List.intercalate " " (map nodeToUniqueName children) ++ " \"service-name\" " ++ (show $ fromMaybe n time)  ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,SlowDataSource,time)) =
    "(slow-get-data " ++ List.intercalate " " (map nodeToUniqueName children) ++ " \"service-name\" " ++ (show $ 10000 + fromMaybe n time)  ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,OtherComputation,time)) =
    "(<$> compute (return " ++ List.intercalate ") (return " (map nodeToUniqueName children) ++ (if length children == 0 then " " else ") (return ") ++ (show $ fromMaybe n time) ++ "))"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,NamedFunction name,time)) =
    "(<$>"  ++ name ++ " (return " ++ List.intercalate ") (return " (map nodeToUniqueName children) ++ (if length children == 0 then " " else ") (return ") ++ (show $ fromMaybe n time) ++ "))"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,SideEffect,time)) =
    "(write-data " ++ List.intercalate " " (map nodeToUniqueName children) ++ " \"service-name\" " ++ (show $ fromMaybe n time) ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,Function,time)) =
    "(ifn" ++ nodeToUniqueName n ++ (if null children then [] else " ") ++ List.intercalate " " (map nodeToUniqueName children) ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,Map,time)) =
    "(map ifn" ++ nodeToUniqueName n ++ " [" ++ List.intercalate " " (map nodeToUniqueName children) ++ "] " ++ ")"
cgNodeToClojureAppFunction graph _ (_,CodeGraphNodeLabel (_,Conditional (CondBranch cond) trueBranch falseBranch,_)) =
    "(if " ++ nodeToUniqueName cond ++ " " ++ List.intercalate " " (map maybeNodeToUniqueName [trueBranch,falseBranch] ) ++ ")"
    where maybeNodeToUniqueName CondNil = "nil"
          maybeNodeToUniqueName (CondBranch node) = nodeToUniqueName node

-- cgNodeToClojureSubFunction :: CodeGraph -> [String] -> Graph.LNode CodeGraphNodeLabel -> String
-- cgNodeToClojureSubFunction _ children (n,CodeGraphNodeLabel (_,DataSource,time)) =
--     "(get-data " ++ List.intercalate " " children ++ " \"service-name\" " ++ (show $ fromMaybe n time)  ++ ")"
-- cgNodeToClojureSubFunction _ children (n,CodeGraphNodeLabel (_,SlowDataSource,time)) =
--     "(slow-get-data " ++ List.intercalate " " children ++ " \"service-name\" " ++ (show $ 10000 + fromMaybe n time)  ++ ")"
-- cgNodeToClojureSubFunction _ children (n,CodeGraphNodeLabel (_,OtherComputation,time)) =
--     "(<$> compute (return " ++ List.intercalate ") (return " children ++ (if length children == 0 then " " else ") (return ") ++ (show $ fromMaybe n time) ++ "))"
-- cgNodeToClojureSubFunction _ children (n,CodeGraphNodeLabel (_,SideEffect,time)) =
--     "(write-data " ++ List.intercalate " " children ++ " \"service-name\" " ++ (show $ fromMaybe n time) ++ ")"
-- cgNodeToClojureSubFunction _ children (n,CodeGraphNodeLabel (_,Function,time)) =
--     "(ifn" ++ nodeToUniqueName n ++ (if null children then [] else " ") ++ List.intercalate " " children ++ ")"
-- cgNodeToClojureSubFunction _ children (n,CodeGraphNodeLabel (_,Map,time)) =
--     "(map ifn" ++ nodeToUniqueName n ++ " [" ++ List.intercalate " " children ++ "] " ++ ")"
-- cgNodeToClojureSubFunction graph children (_,CodeGraphNodeLabel (_,Conditional _ _ _,_)) =
--     if (length names == 3)
--     then "(if " ++ head children ++ " " ++ List.intercalate " " tail children ++ ")"
--     else "(if nil nil nil)"

cgNodeToClojureLetDef :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureLetDef toCode graph = (\x -> (nodeToUniqueName $ fst x) ++ " " ++ (cgNodeToClojureFunction graph (Graph.suc graph $ fst x) x))


toClojureSubFunctions :: (CodeGraph -> String) -> [(CodeGraph, String)] -> String
toClojureSubFunctions _ [] = ""
toClojureSubFunctions toCode subgraphs = List.intercalate "\n" (map (toClojureSubFunction toCode) $ reverse subgraphs)
                                         
toClojureSubFunction :: (CodeGraph -> String) -> (CodeGraph, String) -> String
toClojureSubFunction toCode namedgr@(graph, name) =
    let
        leaves = graphGetLeaves graph
        numLeaves = length leaves
        (head, transformedGraph) = toClojureSubFunctionHead namedgr numLeaves
    in "(" ++ head ++ toCode transformedGraph ++ ")"

toClojureSubFunctionHead :: (CodeGraph, String) -> Int -> (String, CodeGraph)
toClojureSubFunctionHead (graph, name) numLeaves = 
    let 
        parameterNamesList' =  map (\x -> "parameter-" ++ show x) [1..] 
        parameterNamesList = take numLeaves parameterNamesList'
        parameterNames = List.intercalate " " parameterNamesList 
        transformedGraph = cgMakeLvlNamed (maxLevelCG graph) parameterNamesList graph
        head = "defn " ++ name ++ " [ " ++ parameterNames ++ "]\n" 
    in ( head, transformedGraph)

--cgNodeToClojureApplicative :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
--cgNodeToClojureApplicative toCode graph = (\x -> (nodeToUniqueName $ fst x) ++ " " ++ ((cgNodeToClojureFunction toCode) graph (Graph.suc graph $ fst x) x))

