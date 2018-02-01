{-# LANGUAGE OverloadedStrings #-}
module Backends.Haxl where

import           LevelGraphs

import           Backends.Haskell
import qualified Data.Graph.Inductive     as Graph

import           Data.Maybe               (fromMaybe)

import qualified Data.Graph.Inductive     as Graph

import           Backend.Language.Common  as L
import           Backend.Language.Haskell as L
import qualified Data.List                as List
import qualified Data.Map.Strict          as Map
import qualified Data.Tuple               as Tuple


toCodeWith :: ([Symbol] -> [Expr] -> Stmt -> Stmt)
           -> p
           -> NestedCodeGraph
           -> Serialized
toCodeWith f testname = renderProgram . wrapMain . toProgram (convertLevelsHs f)
  where
    wrapMain = alterMain $ \b -> LambdaE $ L.Function ["myEnv"] $ VarE "runHaxl" `ApplyE` b


toHaxlAppCode :: String -> NestedCodeGraph -> Serialized
toHaxlAppCode = toCodeWith parBndApp


toHaxlCode :: String -> NestedCodeGraph -> Serialized
toHaxlCode = toCodeWith parBndDo

