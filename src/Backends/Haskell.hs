{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards     #-}
module Backends.Haskell where

import           Backend.Language.Common  as L
import           Backend.Language.Haskell as L
import           Control.Arrow
import           Data.Char
import qualified Data.Graph.Inductive     as Graph
import           Data.Maybe               (fromMaybe)
import           LevelGraphs              as G


applyMany :: Expr -> [Expr] -> Expr
applyMany = foldl ApplyE

parBndApp :: [Symbol] -> [Expr] -> Stmt -> Stmt
parBndApp vars exprs cont =
  RetS $ applyMany (VarE $ Symbol $ "liftA" ++ show (length vars)) (LambdaE (L.Function vars (DoE cont )) : exprs)


parBndDo :: [Symbol] -> [Expr] -> Stmt -> Stmt
parBndDo vars exprs cont =
  foldr
    (\(var, expr) -> VarD var `BindS` expr)
    cont
    (zip vars exprs)

convertLevelsHs :: ([Symbol] -> [Expr] -> Stmt -> Stmt)
                -> (Int -> [Int])
                -> [[(Int, CodeGraphNodeLabel)]]
                -> Expr
convertLevelsHs parBnd getSuc l
  | [x] <- l = lastLevel x
  | otherwise = DoE $ toStmt l
  where
    toStmt [] = error "empty graph"
    toStmt [lvl] = RetS $ lastLevel lvl
    toStmt (x:xs) =
      case x of
        []  -> error "empty level"
        [f] -> BindS (VarD $ varName $ fst f) (toExpr f) (toStmt xs)
        fs  -> parBnd vars exprs $ toStmt xs
          where
            (vars, exprs) = unzip (map (varName . fst &&& toExpr) fs)
    lastLevel [x] = toExpr x
    lastLevel _   = error "last level must have exactly one node"
    toExpr nlab@(n, lab) = toFunHs nlab (map (VarE . varName) (getSuc n))


toHsName :: String -> Symbol
toHsName = Symbol . f
  where
    f [] = []
    f ('-':xs)
      | (c:cs) <- xs = toUpper c : f cs
      | otherwise = []
    f (c:cs) = c : f cs


toFunHs :: Graph.LNode CodeGraphNodeLabel -> [Expr] -> Expr
toFunHs (n, CodeGraphNodeLabel _ lab _) = case lab of
  Custom "function" -> applyMany (VarE (Symbol $ "fn" ++ show n))
  Custom "map" -> ApplyE (VarE "mapM" `ApplyE` VarE (Symbol $ "fn" ++ show n)) . LitE . LitList
  -- Rename name -> applyMany (VarE "pure")
  Conditional b t e -> applyMany $ IfE (maybe (LitE (LitBool True)) (ApplyE (VarE "(==)") . VarE . varName) b) (f t) (f e)
    where f = maybe (LitE (LitInt 0)) (VarE . varName)
  Custom f -> stdApply (VarE (toHsName f))
  where
    getLen = pure . ApplyE (VarE "sum") . LitE . LitList
    stdApply v = applyMany v . getLen
