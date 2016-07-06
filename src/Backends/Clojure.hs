module Backends.Clojure where

import LevelGraphs

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple

cgNodeToClojureFunction  :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel _ ctype t) =
    case ctype of
        DataSource -> "(get-data " ++ childrenStr ++ " \"service-name\" " ++ timeoutStr  ++ ")"
        SlowDataSource -> "(slow-get-data " ++ childrenStr ++ " \"service-name\" " ++ (show $ 10000 + timeout')  ++ ")"
        OtherComputation -> "(compute " ++ childrenStr ++ " " ++ timeoutStr ++ ")"
        SideEffect -> "(write-data " ++ childrenStr ++ " \"service-name\" " ++ timeoutStr ++ ")"
        NamedFunction name -> "(" ++ name ++ (if null children then [] else " ") ++ childrenStr ++ ")"
        Function -> "(ifn" ++ nodeToUniqueName n ++ (if null children then [] else " ") ++ childrenStr ++ ")"
        Map -> "(map ifn" ++ nodeToUniqueName n ++ " (vector " ++ childrenStr ++ ") " ++ ")"
        Conditional (CondBranch cond) trueBranch falseBranch ->
          "(if " ++ nodeToUniqueName cond ++ " " ++ List.intercalate " " (map maybeNodeToUniqueName [trueBranch,falseBranch] ) ++ ")"
            where maybeNodeToUniqueName CondNil = "nil"
                  maybeNodeToUniqueName (CondBranch node) = nodeToUniqueName node
        Rename name -> name
  where
    childrenStr = List.intercalate " " (map nodeToUniqueName children)
    timeout' = fromMaybe n t
    timeoutStr = show timeout'


cgNodeToClojureAppFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureAppFunction graph children (n,CodeGraphNodeLabel _ ctype t) =
    case ctype of
        DataSource -> "(get-data " ++ childrenStr ++ " \"service-name\" " ++ timeoutStr  ++ ")"
        SlowDataSource -> "(slow-get-data " ++ childrenStr ++ " \"service-name\" " ++ (show $ 10000 + timeout')  ++ ")"
        OtherComputation -> "(compute " ++ childrenStr ++ " " ++ timeoutStr ++ ")"
        NamedFunction name -> "(" ++ name ++ " " ++ childrenStr ++ " " ++ timeoutStr ++ ")"
        SideEffect -> "(write-data " ++ childrenStr ++ " \"service-name\" " ++ timeoutStr ++ ")"
        Function -> "(ifn" ++ nodeToUniqueName n ++ (if null children then [] else " ") ++ childrenStr ++ ")"
        Map -> "(map ifn" ++ nodeToUniqueName n ++ " (vector " ++ childrenStr ++ ") " ++ ")"
        Conditional (CondBranch cond) trueBranch falseBranch ->
            "(if " ++ nodeToUniqueName cond ++ " " ++ List.intercalate " " (map maybeNodeToUniqueName [trueBranch,falseBranch] ) ++ ")"
          where
              maybeNodeToUniqueName CondNil = "nil"
              maybeNodeToUniqueName (CondBranch node) = nodeToUniqueName node
        Rename name -> name
  where
    childrenStr = List.intercalate " " (map nodeToUniqueName children)
    timeout' = fromMaybe n t
    timeoutStr = show timeout'


cgNodeToClojureLetDef :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureLetDef toCode graph x@(x1,_) = nodeToUniqueName x1 ++ " " ++ cgNodeToClojureFunction graph (Graph.suc graph x1) x

toClojureFunctions :: (CodeGraph -> String) -> [(CodeGraph, FnName, Arity)] -> String
toClojureFunctions toCode subGraphs = toClojureSubFunctions toCode subGraphs (toClojureSubFunctionHead "defn")

toClojureSubFunctions :: (CodeGraph -> String) -> [(CodeGraph, FnName, Arity)] -> HeadFunction -> String
toClojureSubFunctions _ [] _ = ""
toClojureSubFunctions toCode subgraphs hdFun = List.intercalate "\n" $ map (toClojureSubFunction toCode hdFun) $ reverse subgraphs

toClojureSubFunction :: (CodeGraph -> String) -> HeadFunction -> (CodeGraph, FnName, Arity) -> String
toClojureSubFunction toCode hdFun namedgr =
    let
        (head, transformedGraph) = hdFun namedgr
    in "(" ++ head ++ toCode transformedGraph ++ ")"

type HeadFunction =  (CodeGraph, String, Arity) -> (String, CodeGraph)
toClojureSubFunctionHead :: String -> HeadFunction
toClojureSubFunctionHead defName (graph, name, arity) =
    let
        parameterNamesList =  map (\x -> "parameter-" ++ show x) [1..arity]
        parameterNames = List.intercalate " " parameterNamesList
        transformedGraph = cgMakeLvlNamed (maxLevelCG graph) parameterNamesList graph
        head = defName ++ " " ++ name ++ " [" ++ parameterNames ++ "]\n"
    in ( head, transformedGraph)

--cgNodeToClojureApplicative :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
--cgNodeToClojureApplicative toCode graph = (\x -> (nodeToUniqueName $ fst x) ++ " " ++ ((cgNodeToClojureFunction toCode) graph (Graph.suc graph $ fst x) x))
