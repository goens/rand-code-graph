{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Backends.Ohua (toOhuaAppCodeWrapped, toOhuaCodeWrapped) where

import           Backends.Clojure           (toFunClj)
import           LevelGraphs

import           Backend.Language.Clojure
import           Backend.Language.Common
import           Data.Graph.Inductive       (Gr)
import qualified Data.Graph.Inductive       as Graph
import           Data.Graph.Inductive.Graph as G
import           Data.Maybe                 (fromMaybe)


toOhuaCodeWith :: ((Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> Expr) -> String -> NestedCodeGraph -> Serialized
toOhuaCodeWith f testname gr = renderProgram (RenderOpts "defalgo" "algo") $ wrapMain $ toProgram f gr
  where
    wrapMain = alterMain $ \b -> Form [Sym "defn", Sym (Symbol testname), Vect [], Form [Sym "ohua", b]]


toOhuaAppCodeWrapped :: String -> NestedCodeGraph -> Serialized
toOhuaAppCodeWrapped = toOhuaCodeWith convertLevelsApp


toOhuaCodeWrapped :: String -> NestedCodeGraph -> Serialized
toOhuaCodeWrapped = toOhuaCodeWith convertLevels


convertLevelsWith :: ((LNode CodeGraphNodeLabel -> Expr) -> [(Int, CodeGraphNodeLabel)] -> [(Expr, Expr)])
                  -> (Node -> [Node])
                  -> [[(Int, CodeGraphNodeLabel)]] -> Expr
convertLevelsWith parBind getSuc lvls = mkLet assigns [finalExpr]
  where
    toFun n = toFunOhua n (map (Sym . varName) $ getSuc (fst n))
    (assigns, finalExpr) = toAssign [] lvls
    toAssign _ [] = error "empty"
    toAssign l [x] =
      case x of
        [x] -> (l, toFun x)
        _   -> error "last level must have exactly one node"
    toAssign l (x:xs) = toAssign (l ++ e) xs
      where
        e = case x of
              []          -> error "empty assignment"
              [n@(id, _)] -> [(Sym $ varName id, toFun n)]
              fs          -> parBind toFun fs

convertLevels :: (Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> Expr
convertLevels = convertLevelsWith $ \toFun -> map $ \n@(id, _) -> (Sym $ varName id, toFun n)

convertLevelsApp :: (Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> Expr
convertLevelsApp = convertLevelsWith $ \toFun fs -> [(Vect (map (Sym . varName . fst) fs), Form $ Sym "mvector" : map toFun fs)]


toFunOhua :: LNode CodeGraphNodeLabel -> [Expr] -> Expr
toFunOhua node@(n, CodeGraphNodeLabel _ lab _) children =
  case lab of
    Custom "map"
      | null children -> Nil -- removes empty maps
      | otherwise -> Form [ Sym "count"
                          , Form [ Sym "smap"
                                 , Sym $ fnName n
                                 , Form $ Sym "mvector" : children
                                 ]
                          ]
    _ -> toFunClj node children

