{-# LANGUAGE OverloadedStrings #-}
module Backends.Muse where

import           Backend.Language.Clojure
import           Backend.Language.Common
import           Backends.Clojure         (toFunClj)
import           Data.Default.Class
import           Data.Graph.Inductive     as Graph
import           Data.Maybe               (fromMaybe)
import           LevelGraphs



toCodeWith :: ((Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> Expr) -> String -> NestedCodeGraph -> Serialized
toCodeWith f testname = renderProgram def . wrapMain . toProgram f
  where
    wrapMain = alterMain $ \b -> Form [Sym "defn", Sym (Symbol testname), Vect [], Form [Sym "run!!", b]]

toMuseAppCode :: String -> NestedCodeGraph -> Serialized
toMuseAppCode = toCodeWith convertLevelsApp


toMuseCode :: String -> NestedCodeGraph -> Serialized
toMuseCode = toCodeWith convertLevels


convertLevelsWith parBind getSuc lvls = mkLetWith "mlet" assigns [finalExpr]
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
              []          -> error "empty assignment"
              [n@(id, _)] -> [(Sym $ varName id, toFun n)]
              fs          -> parBind toFun fs

convertLevels :: (Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> Expr
convertLevels = convertLevelsWith $ \toFun -> map $ \n@(id, _) -> (Sym $ varName id, toFun n)

convertLevelsApp :: (Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> Expr
convertLevelsApp = convertLevelsWith $ \toFun fs -> [(Vect (map (Sym . varName . fst) fs), Form $ Sym "<$>":  Sym "clojure.core/vector" : map toFun fs)]

toFunMuse node@(n, CodeGraphNodeLabel _ lab _) children =
  case lab of
    Custom "map"      -> Form $ Sym "traverse": Sym (fnName n):children
    Conditional _ _ _ -> Form [Sym "return", defer]
    _                 -> defer
  where
    defer = toFunClj node children
