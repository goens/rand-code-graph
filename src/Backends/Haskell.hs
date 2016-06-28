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
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,DataSource,time)) =
    "getData \"service-name\" [" ++ (show $ fromMaybe n time) ++ helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,SlowDataSource, time)) =
    "slowGetData \"service-name\" [" ++ (show $ 10000 + fromMaybe n time) ++ helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,OtherComputation,time)) =
    "compute [" ++ (show $ fromMaybe n time) ++  helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,Function,time)) =
    "ifn" ++ nodeToUniqueName n ++ " "  ++ List.intercalate " " (map nodeToUniqueName children)
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,Map,time)) =
    "map ifn" ++ nodeToUniqueName n ++ " [" ++ (show $ fromMaybe n time) ++ helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,SideEffect,time)) =
    "writeData \"service-name\" [" ++ (show $ fromMaybe n time) ++  helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction _ (_,CodeGraphNodeLabel (_,Conditional cond trueBranch falseBranch,_)) =
    "if " ++ (maybeNodeToUniqueName cond) ++ " then " ++ (maybeNodeToUniqueName trueBranch) ++ " else " ++  (maybeNodeToUniqueName falseBranch)
           where maybeNodeToUniqueName CondNil = "nil"
                 maybeNodeToUniqueName (CondBranch node) = nodeToUniqueName node


cgNodeToHaskellDoBind:: CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellDoBind graph = (\x -> (nodeToUniqueName $ fst x) ++ " <- " ++ (cgNodeToHaskellFunction (Graph.suc graph $ fst x) x))


