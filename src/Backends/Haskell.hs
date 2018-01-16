{-# LANGUAGE OverloadedStrings #-}
module Backends.Haskell where

import           Data.Maybe           (fromMaybe)
import           LevelGraphs as G
import qualified Data.Graph.Inductive as Graph
import Backend.Language.Common as L
import Backend.Language.Haskell as L
import Control.Arrow


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
    lastLevel _ = error "last level must have exactly one node"
    toExpr nlab@(n, lab) = toFunHs nlab (map (VarE . varName) (getSuc n))


toFunHs :: Graph.LNode CodeGraphNodeLabel -> [Expr] -> Expr
toFunHs (n, CodeGraphNodeLabel _ lab _) = case lab of
  DataSource -> stdApply (VarE "getData")
  SideEffect -> stdApply (VarE "writeData")
  OtherComputation -> stdApply (VarE "compute")
  G.NamedFunction f -> applyMany (VarE (Symbol f))
  G.Function -> applyMany (VarE (Symbol $ "fn" ++ show n))
  Map -> ApplyE (VarE "mapM" `ApplyE` VarE (Symbol $ "fn" ++ show n)) . LitE . LitList
  Rename name -> applyMany (VarE "pure")
  Conditional b t e -> applyMany $ IfE (maybe (LitE (LitBool True)) (ApplyE (VarE "(==)") . VarE . varName) b) (f t) (f e)
    where f = maybe (LitE (LitInt 0)) (VarE . varName)
  where
    getLen = pure . ApplyE (VarE "sum") . LitE . LitList
    stdApply v = applyMany v . getLen
