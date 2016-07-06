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
        Function -> "ifn" ++ nodeToUniqueName n ++ " "  ++ timeoutChildrenList
        Map -> "map ifn" ++ nodeToUniqueName n ++ " " ++ timeoutChildrenList
        SideEffect -> "writeData \"service-name\" " ++ timeoutChildrenList
        Conditional cond trueBranch falseBranch ->
            "if " ++ maybe "True" nodeToUniqueName cond ++ " then " ++ f trueBranch ++ " else " ++ f falseBranch
          where f = maybe "0" nodeToUniqueName
        Rename name -> name
  where
    timeout' = fromMaybe n t
    timeoutStr = show timeout'
    listWTimeout t = "[" ++ List.intercalate ", " (t: map nodeToUniqueName children) ++ "]"
    timeoutChildrenList = listWTimeout timeoutStr

cgNodeToHaskellDoBind:: CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellDoBind graph x@(x1,_) = nodeToUniqueName x1 ++ " <- " ++ cgNodeToHaskellFunction (Graph.suc graph x1) x
