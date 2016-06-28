module Unit where

import LevelGraphs
import Backends.Ohua
import Backends.Haxl
import Backends.Muse

------------------------------------------------------------
-- Simple Examples
------------------------------------------------------------

manualExample :: LevelGraph
manualExample = Graph.mkGraph [(1,1),(2,1),(3,2),(4,2),(5,2)] [(1,3,()),(1,4,()),(1,5,()),(2,3,()),(2,4,()),(2,5,())]

someExample :: CodeGraph
someExample = makeCodeGraph DataSource $ joinLevelGraph (nullLevelGraph 2 2) (nullLevelGraph 3 2)

exampleMap :: Map.Map (Int,Int) Double
--exampleMap = Map.fromList [ ((1,2),0.5), ((1,3),0.2), ((1,4),0.05) ]
exampleMap = Map.fromList [ ((a,b), (1 / 2^(b-a))) | a<- [1..50], b<-[1..50], a<b]

randomExample :: MonadRandom m => m LevelGraph
randomExample = joinLevelGraphRandom exampleMap (nullLevelGraph 1 2) (nullLevelGraph 2 3)

randomCodeGraphExample :: MonadRandom m => m CodeGraph
randomCodeGraphExample = genRandomCodeGraph exampleMap [0.4,0.1, 0.2, 0.2] [2,2,3]

randomCodeGraphExampleVarLength :: MonadRandom m => Int -> m CodeGraph
randomCodeGraphExampleVarLength n = (sequence $ replicate n (Control.Monad.Random.fromList [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ])) >>= genRandomCodeGraph exampleMap [0.4,0.1]

-- Fixme: get examples up there working again

someExampleStrings :: forall m. MonadRandom m => m String
someExampleStrings =
    let
        emptyMonadList :: m CodeSubGraphs
        emptyMonadList = return []
        --randomEx :: m NestedCodeGraph
        randomEx = sequenceT (randomCodeGraphExample :: m CodeGraph, emptyMonadList)
    in liftM (concatenateTests toOhuaCodeWrapped ) $ sequence (List.replicate 10 randomEx )

--someExampleStringsVarLength :: MonadRandom m => m String
--someExampleStringsVarLength = liftM (concatenateTests toOhuaCodeWrapped) $ sequence (map (\(x,_) -> (randomCodeGraphExampleVarLength x, []) [1..20]))



------------------------------------------------------------
-- Test Area
------------------------------------------------------------

--main = do
--  str <- Control.Monad.Random.evalRandIO someExampleStringsVarLength
--  putStrLn str
