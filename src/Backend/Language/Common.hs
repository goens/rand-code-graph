{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards           #-}
module Backend.Language.Common where

import           Data.Default.Class
import           Data.Function
import           Data.String
import           Data.Text.Prettyprint.Doc
import           Data.Graph.Inductive.Graph
import LevelGraphs (CodeGraphNodeLabel, NestedCodeGraph, cGraphLevelSort)

type Serialized = Doc ()

newtype Symbol = Symbol { symToStr :: String } deriving IsString

type Type = ()

data Typed a = Typed Type a

instance Pretty Symbol where
  pretty = pretty . symToStr

data Program lang = Program
  { functions :: [NamedFunction lang]
  , main      :: Function lang
  }

data NamedFunction lang = NamedFunction
  { functionName          :: Symbol
  , namedFunctionFunction :: Function lang
  }

data Function lang = Function
  { parameters   :: [Symbol]
  , functionBody :: lang
  }


alterMain :: (lang -> lang) -> Program lang -> Program lang
alterMain f prog = prog { main = (main prog) { functionBody = f (functionBody (main prog)) } }


varName :: Int -> Symbol
varName n = Symbol $ "local" ++ show n


fnName :: Int -> Symbol
fnName n = Symbol $ "fn" ++ show n


toProgram :: ((Node -> [Node]) -> [[LNode CodeGraphNodeLabel]] -> lang)
          -> NestedCodeGraph
          -> Program lang
toProgram convertLevels (gr, subGrs) =
  Program
    (map (\(g, n, a) -> toNamedFunction convertLevels (Symbol n) a g) subGrs)
    (toFunction convertLevels 0 gr)

mkNameList strs =
  [ Symbol $ c ++ maybe "" show n
  | n <- Nothing : map Just [0..] :: [Maybe Int]
  , c <- strs
  ]

nameList = mkNameList $ map pure ['a' .. 'z']

toNamedFunction convertLevels n a g = NamedFunction n $ toFunction convertLevels a g


toFunction convertLevels arity gr =
  Function
    (take arity nameList)
    $ case reverse $ cGraphLevelSort gr of
        []    -> error "empty graph"
        l -> convertLevels (suc gr) l
