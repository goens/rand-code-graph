module Backends.Haskell where

import LevelGraphs
import           Data.Maybe           (fromMaybe)

import qualified Data.Graph.Inductive as Graph

import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple

helperNodeToHaskellFunction :: [Graph.Node] -> String
helperNodeToHaskellFunction children = listStart ++ List.intercalate ", " (map nodeToUniqueName children)
    where listStart = if null children then "" else ", "

cgNodeToHaskellFunction :: [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel _ ctype t) =
    case ctype of
        DataSource -> "getData \"service-name\" " ++ timeoutChildrenList
        SlowDataSource -> "slowGetData \"service-name\" " ++ listWTimeout (show $ 10000 + timeout')
        OtherComputation -> "compute " ++ timeoutChildrenList
        NamedFunction name ->  name ++ " " ++ paramList
        Function -> "ifn" ++ nodeToUniqueName n ++ " "  ++ paramList
        Map -> "map ifn" ++ nodeToUniqueName n ++ " " ++ timeoutChildrenList
        SideEffect -> "writeData \"service-name\" " ++ timeoutChildrenList
        Conditional cond trueBranch falseBranch ->
            "if " ++ maybe "True" nodeToUniqueName cond ++ " then return " ++ f trueBranch ++ " else return " ++ f falseBranch
          where f = maybe "0" nodeToUniqueName
        Rename name -> "return " ++ name
  where
    timeout' = fromMaybe n t
    timeoutStr = show timeout'
    listWTimeout t = "[" ++ List.intercalate ", " (t: map nodeToUniqueName children) ++ "]"
    paramList = List.intercalate " " (map nodeToUniqueName children)
    timeoutChildrenList = listWTimeout timeoutStr

cgNodeToHaskellDoBind:: CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellDoBind graph x@(x1,_) = nodeToUniqueName x1 ++ " <- " ++ cgNodeToHaskellFunction (Graph.suc graph x1) x

toHaskellDoCode :: CodeGraph -> String
toHaskellDoCode graph = helperToHaskellDoCode nodes ++ "\n"
    where
      nodes = reverse $ cGraphTopSort graph --bottom up
      trSpace = "  "
      helperToHaskellDoCode ns = (concatMap (\x -> trSpace ++ cgNodeToHaskellDoBind graph x ++ "\n") ns) ++ trSpace ++ "return " ++ nodeToUniqueName (fst $ last ns) ++ "\n"


toHaskellSubFunctions :: (CodeGraph -> String) -> [(CodeGraph, FnName, Arity)] -> String
toHaskellSubFunctions _ [] = ""
toHaskellSubFunctions toCode subgraphs = List.intercalate "\n" $ map (toHaskellSubFunction toCode) $ reverse subgraphs

toHaskellSubFunction :: (CodeGraph -> String) -> (CodeGraph, FnName, Arity) -> String
toHaskellSubFunction toCode namedgr =
    let
        (head, transformedGraph) = toHaskellSubFunctionHead namedgr
    in head ++ toCode transformedGraph

toHaskellSubFunctionHead :: (CodeGraph, String, Arity) -> (String, CodeGraph)
toHaskellSubFunctionHead (graph, name, arity) =
    let
        parameterNamesList =  map (\x -> "parameter_" ++ show x) [1..arity]
        parameterNames = List.intercalate " " parameterNamesList
        transformedGraph = cgMakeLvlNamed (maxLevelCG graph) parameterNamesList graph
        head = name ++ " " ++ parameterNames ++ " = do\n"
    in (head, transformedGraph)
