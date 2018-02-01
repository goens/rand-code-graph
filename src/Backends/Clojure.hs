{-# LANGUAGE OverloadedStrings #-}
module Backends.Clojure where

import           Backend.Language.Clojure as L
import           Backend.Language.Common  as L
import           Data.Default.Class
import           Data.Graph.Inductive     as Graph
import           Data.Maybe               (fromMaybe)
import           LevelGraphs              as G


toClojureCode :: String -> NestedCodeGraph -> Serialized
toClojureCode _ = renderProgram def . toProgram convertLevels

convertLevels :: (Node -> [Node]) -> [[Graph.LNode CodeGraphNodeLabel]] -> Expr
convertLevels getSuc lvls = mkLet assigns [finalExpr]
  where
    toFun n = toFunClj n (map (Sym . varName) $ getSuc (fst n))
    (assigns, finalExpr) = toAssign [] lvls
    toAssign _ [] = error "empty"
    toAssign l [x] =
      case x of
        [x] -> (l, toFun x)
        _   -> error "last level must have exactly one node"
    toAssign l (x:xs) = toAssign (e ++ l) xs
      where
        e = case x of
              [] -> error "empty assignment"
              [n@(id, _)] -> [(Sym $ varName id, toFun n)]
              fs ->  map (\n@(id, _) -> (Sym $ varName id, toFun n)) fs

toFunClj :: Graph.LNode CodeGraphNodeLabel -> [Expr] -> Expr
toFunClj node@(n, CodeGraphNodeLabel _ lab _) children =
  case lab of
    Custom "function" -> Form $ Sym (fnName n) : children
    Custom "map" -> Form [Sym "count", Form [Sym "map", Sym (fnName n), Form $ Sym "vector" : children]]
    Conditional cond true false -> mkIf (maybe (Bool True) mkRef cond) (maybe Nil mkRef true) (fmap mkRef false)
      where mkRef = Sym . varName
    Custom f -> Form $ Sym (Symbol f) : children
