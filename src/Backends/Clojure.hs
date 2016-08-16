module Backends.Clojure where

import           LevelGraphs

import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import           Backends.Common
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple
import           Debug.Trace
import           Text.Printf


-- TODO Add map index like in Haskell for data fetches
cgNodeToClojureFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureFunction _ children (n,label) =
    case computationType label of
        DataSource -> printf "(get-data %s \"service-name\" %s)" childrenStr timeout'
        SlowDataSource -> printf "(slow-get-data %s \"service-name\" %d)" childrenStr (10000 + timeout')
        OtherComputation -> printf "(compute %s %d)" childrenStr timeout'
        SideEffect -> printf "(write-data %s \"service-name\" %d)" childrenStr timeout'
        NamedFunction name -> printf "(%s %s)" name childrenStr
        Function -> printf "(ifn%s %s)" (nodeToUniqueName n) childrenStr
        Map -> printf "(count (map if%s (vector %s)))" (nodeToUniqueName n) childrenStr
        Conditional cond _ _ ->
            if shouldInlineIfBranches undefined
                then printf "(if %s %s %s)" condBr callThen callElse
                else printf
                      "(let [%s %s\n%s %S] (if %s %s %s))"
                      thenTemp callThen
                      elseTemp callElse
                      condBr
                      thenTemp elseTemp
          where
            thenTemp = "then" ++ nodeToUniqueName n
            elseTemp = "else" ++ nodeToUniqueName n
            condBr = maybe "nil" nodeToUniqueName cond
            callFn prefix = "(ifn" ++ prefix ++ nodeToUniqueName n ++ " " ++ childrenStr ++ ")"
            callThen = callFn "then"
            callElse = callFn "else"
        Rename name -> name
  where
    childrenStr = List.intercalate " " (map nodeToUniqueName children)
    timeout' = fromMaybe n (timeout label)


cgNodeToClojureAppFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureAppFunction = cgNodeToClojureFunction


cgNodeToClojureLetDef :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureLetDef toCode graph x@(x1,_) = nodeToUniqueName x1 ++ " " ++ cgNodeToClojureFunction graph (Graph.suc graph x1) x

toClojureFunctions :: (CodeGraph -> String) -> CodeSubGraphs -> String
toClojureFunctions toCode subGraphs = toClojureSubFunctions toCode subGraphs (toClojureSubFunctionHead "defn")

toClojureSubFunctions :: (CodeGraph -> String) -> CodeSubGraphs -> HeadFunction -> String
toClojureSubFunctions _ [] _ = ""
toClojureSubFunctions toCode subgraphs hdFun = List.intercalate "\n" $ map (toClojureSubFunction toCode hdFun) $ reverse subgraphs

toClojureSubFunction :: (CodeGraph -> String) -> HeadFunction -> FunctionGraph -> String
toClojureSubFunction toCode hdFun namedgr =
    let
        (head, transformedGraph) = hdFun namedgr
    in "(" ++ head ++ toCode transformedGraph ++ ")"

type HeadFunction =  FunctionGraph -> (String, CodeGraph)
toClojureSubFunctionHead :: String -> HeadFunction
toClojureSubFunctionHead defName (FunctionMeta{fnArity=arity, fn=graph, fnName=name}) =
    let
        parameterNamesList =  map (\x -> "parameter-" ++ show x) [1..arity]
        parameterNames = List.intercalate " " parameterNamesList
        transformedGraph = cgMakeLvlNamed (maxLevelCG graph) parameterNamesList graph
        head = defName ++ " " ++ name ++ " [" ++ parameterNames ++ "]\n"
    in ( head, transformedGraph)

--cgNodeToClojureApplicative :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
--cgNodeToClojureApplicative toCode graph = (\x -> (nodeToUniqueName $ fst x) ++ " " ++ ((cgNodeToClojureFunction toCode) graph (Graph.suc graph $ fst x) x))
