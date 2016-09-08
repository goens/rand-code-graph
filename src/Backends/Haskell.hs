module Backends.Haskell where

import           Data.Maybe           (fromMaybe)
import           LevelGraphs

import qualified Data.Graph.Inductive as Graph

import           Backends.Common
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple
import           Text.Printf


helperNodeToHaskellFunction :: [Graph.Node] -> String
helperNodeToHaskellFunction children = listStart ++ List.intercalate ", " (map nodeToUniqueName children)
    where listStart = if null children then "" else ", "

cgNodeToHaskellFunction :: [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellFunction children (n,label) =
    case computationType label of
        DataSource ->
          printf
            "getData \"service-name\" (%d : %s ++ [%s])"
            timeout'
            (fromMaybe "[]" $ mapIndex label)
            (List.intercalate ", " $ map nodeToUniqueName children)
        SlowDataSource ->
          printf
            "slowGetData \"service-name\" (%d : %s [%s])"
            (10000 + timeout')
            (fromMaybe "[]" $ mapIndex label)
            (List.intercalate ", " $ map nodeToUniqueName children)
        OtherComputation -> "compute " ++ timeoutChildrenList
        -- HACK (temporary)
        -- NamedFunction name ->  name ++ " " ++ paramList
        NamedFunction name -> printf "%s (length [%s])" name (List.intercalate ", " (map nodeToUniqueName children))
        -- HACK (temporary)
        -- Function -> "ifn" ++ nodeToUniqueName n ++ " "  ++ paramList
        Function -> printf "ifn%s (length [%s])" (nodeToUniqueName n) (List.intercalate ", " (map nodeToUniqueName children))
        Map ->
          printf
            "fmap length (mapM (uncurry ifn%s) [%s])"
            (nodeToUniqueName n)
            (List.intercalate ", " l)
          where l = [ "(" ++ maybe "" (++ " ++ ") (mapIndex label) ++ "[" ++ show index ++ "]" ++ ", " ++ val ++ ")"
                    | (index, val) <- zip [0..] (map nodeToUniqueName children)
                    ]
        SideEffect -> "writeData \"service-name\" " ++ timeoutChildrenList
        Conditional cond _ _ ->
            if shouldInlineIfBranches undefined
                then printf "(if %s then %s else %s)" condBr callThen callElse
                else
                  printf
                    "(do {(%s, %s) <- (,) <$> %s <*> %s; return (if %s then %s else %s)})"
                    thenTemp elseTemp
                    callThen callElse
                    condBr
                    thenTemp elseTemp
          where
            thenTemp = "then" ++ nodeToUniqueName n
            elseTemp = "else" ++ nodeToUniqueName n
            condBr = maybe "True" ( (++ " == 1") . nodeToUniqueName) cond
            callFn prefix = "ifn" ++ prefix ++ nodeToUniqueName n ++ " " ++ paramList
            callThen = callFn "then"
            callElse = callFn "else"
        Rename name -> "return " ++ name
  where
    timeout' = fromMaybe n (timeout label)
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


toHaskellSubFunctions :: (CodeGraph -> String) -> CodeSubGraphs -> String
toHaskellSubFunctions _ [] = ""
toHaskellSubFunctions toCode subgraphs = List.intercalate "\n" $ map (toHaskellSubFunction toCode) $ reverse subgraphs

toHaskellSubFunction :: (CodeGraph -> String) -> FunctionGraph -> String
toHaskellSubFunction toCode namedgr =
    let
        (head, transformedGraph) = toHaskellSubFunctionHead namedgr
    in head ++ toCode transformedGraph

toHaskellSubFunctionHead :: FunctionGraph -> (String, CodeGraph)
toHaskellSubFunctionHead FunctionMeta{fn=graph, fnName=name, fnArity=arity, fnIsMapped=mapped} =
    let
        parameterNamesList = map (\x -> "parameter_" ++ show x) [1..arity]

        paramTypes
          | mapped = " [Int] " : bl
          | otherwise = bl
          where bl = List.replicate arity "Int"
        typeSignature = name ++ ":: " ++ List.intercalate " -> " (paramTypes ++ ["GenHaxl u Int"])
        parameterNames = List.intercalate " "$ if mapped then "mapIndex" : parameterNamesList else parameterNamesList
        transformedGraph
          | mapped = Graph.nmap (\l -> l {mapIndex=Just "mapIndex"}) g
          | otherwise = g
          where g = cgMakeLvlNamed (maxLevelCG graph) parameterNamesList graph
        head = typeSignature ++ "\n" ++ name ++ " " ++ parameterNames ++ " = do\n"
    in (head, transformedGraph)
