{-# LANGUAGE ViewPatterns  #-}
-- | Code for generating a maze from a randomized graph traversal. A
--   maze is just represented by a list of the walls to draw.
-- 
--   Andres Goens, TU Dresden, Chair for Compiler Construction
--   andres.goens@tu-dresden.de

module LevelGraphs where

import           Control.Monad        (foldM,liftM,liftM2,mapM,guard,MonadPlus)
import           Control.Monad.Random (MonadRandom,fromList)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr)
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map


data ComputationType = DataSource | OtherComputation deriving (Show, Eq)
data Statistics = Statistics (Int, Double) deriving (Show, Eq) -- Mu, Sigma


type Level = Int
type LevelGraph = Gr Level () -- There can only be edges (a,b) if level a < level b
type CodeGraph = Gr (Level,ComputationType) ()

------------------------------------------------------------
-- General helper functions
------------------------------------------------------------

--uses the fact that Graph.Node = Int
intMapNode :: (Int -> Int) -> Graph.Context a b -> Graph.Context a b
intMapNode f (p,n,l,s) = (f' p, f n, l, f' s)
                     where f' context = List.map (\(b,node) -> (b,f node)) context


intMap :: (Int -> Int) -> Gr a b -> Gr a b
intMap f gr = Graph.buildGr $ List.map (intMapNode f) $ Graph.ufold (:) [] gr

------------------------------------------------------------
-- Random-monad helper functions
------------------------------------------------------------

rconcat :: MonadRandom m => m [a] -> m [a] -> m [a]
rconcat = liftM2 (++)

rguard ::MonadRandom m => MonadPlus n => m Bool -> m (n ())
rguard = (liftM guard)

trueWithProb :: MonadRandom m => Double -> m Bool
trueWithProb p = let p' = toRational p in Control.Monad.Random.fromList [ (True, p'), (False, (1 - p')) ]

dsWithProb :: MonadRandom m => Double -> m ComputationType
dsWithProb p = let p' = toRational p in Control.Monad.Random.fromList [ (DataSource, p'), (OtherComputation, (1 - p')) ]


emptyWithProb :: MonadRandom m => Double -> [a] -> m [a]
emptyWithProb p list = let p' = toRational p in Control.Monad.Random.fromList [ (list, p'), ([], (1 - p')) ]

-- caution: because I'm lazy and don't want to properly use tranformers, this is probably inefficient
elemWithProb :: MonadRandom m => Double -> [a] -> m [a] 
elemWithProb _ [] = return []
elemWithProb p (e:es) =  liftM2 (++) (emptyWithProb p [e]) (elemWithProb p es)

elemWithProbMap :: MonadRandom m => [(a,Double)] -> m [a] 
elemWithProbMap [] = return []
elemWithProbMap ((e,p):es) =  liftM2 (++) (emptyWithProb p [e]) (elemWithProbMap es)



------------------------------------------------------------
-- Conversion Functions
------------------------------------------------------------


addLevelContext :: Level -> Graph.Context () b -> Graph.Context Level b
addLevelContext level (pre,node,(),after) = (pre,node,level,after)

addCodeContext :: ComputationType -> Graph.Context Level b -> Graph.Context (Level,ComputationType) b
addCodeContext ctype (pre,node,lvl,after) = (pre,node,(lvl,ctype),after)

graph2LevelGraph ::  Level -> Gr () () -> LevelGraph
graph2LevelGraph level gr = Graph.buildGr (List.map (addLevelContext level) unfolded)
    where unfolded = Graph.ufold (:) [] gr

qlEdge2Edge :: Graph.LEdge () -> Graph.Edge
qlEdge2Edge (a,b,()) = (a,b)

makeCodeGraph :: ComputationType -> LevelGraph -> CodeGraph
makeCodeGraph ctype = Graph.nmap (\l -> (l,ctype) ) 

-- Makes a level graph into a code graph with a probability p for being a DataSource for every node
makeRandomCodeGraph :: MonadRandom m => Double -> LevelGraph -> m CodeGraph 
makeRandomCodeGraph probDataSource gr = liftM Graph.buildGr transformed
    where 
      unfolded = Graph.ufold (:) [] gr
      transformed = flip Control.Monad.mapM unfolded $ \ctx -> do ctype <- dsWithProb probDataSource
                                                                  return $ addCodeContext ctype ctx

makeGraphRooted ::  a -> Gr a () -> Gr a ()
makeGraphRooted rootlabel graph = Graph.mkGraph (oldNodes ++ [(unocupied,rootlabel)]) (oldEdges ++ newEdges) --the problem is that unocupied has no label!
--makeGraphRooted _ graph  = newEdges
    where
      unocupied = (+1) $ snd $ Graph.nodeRange graph
      oldEdges = Graph.labEdges graph
      oldNodes = Graph.labNodes graph
      oldNodes' = map fst $ Graph.labNodes graph
      newEdges = [ (unocupied,node,()) | node <- oldNodes', (null $ Graph.pre graph node) ]



------------------------------------------------------------
-- Basic graph families
------------------------------------------------------------

--uses the fact that Graph.Node = Int
fullGraph :: Int -> Gr () ()
fullGraph n = Graph.mkUGraph vs (concat $ map (\x -> [(x,a) | a <- [(x+1)..n] ] ++  [(a,x) | a <- [(x+1)..n] ] ) vs)
    where vs = [1..n]

trivialGraph :: Int -> Gr () ()
trivialGraph n = Graph.mkUGraph [1..n] []

trivialLevelGraph :: Level -> Int -> LevelGraph
trivialLevelGraph level n = Graph.buildGr $ List.map (\x -> ([],x,level,[])) [1..n]

------------------------------------------------------------
-- Graph generating functions from graphs
------------------------------------------------------------

joinGraph :: Gr a () -> Gr a () -> Gr a ()
joinGraph g h = Graph.mkGraph (nodesg ++ nodeshmoved) (edgesg ++ edgesprod ++ edgeshmoved)
    where nodesg = Graph.labNodes g
          hmoved = intMap (+ (snd $ Graph.nodeRange g) ) h -- move nodes from g to start at 1 + (maxnode h)
          nodeshmoved = Graph.labNodes hmoved
          edgesg = Graph.labEdges g
          edgeshmoved = Graph.labEdges hmoved
          edgesprod = List.nub [ (a,b,()) | (a,_) <-nodesg, (b,_) <- nodeshmoved ]

joinLevelGraph :: LevelGraph -> LevelGraph -> LevelGraph
joinLevelGraph g h = Graph.mkGraph (nodesg ++ nodeshmoved) (edgesg ++ edgesprod ++ edgeshmoved)
--joinLevelGraph g h = (edgesg , edgesprod , edgeshmoved)
    where nodesg = Graph.labNodes g
          hmoved = intMap (+ (snd $ Graph.nodeRange g) ) h -- move nodes from g to start at 1 + (maxnode h)
          nodeshmoved = Graph.labNodes hmoved
          edgesg = Graph.labEdges g
          edgeshmoved = Graph.labEdges hmoved
          edgesprod = List.nub $ [ (a,b,()) | (a,l1) <-nodesg, (b,l2) <- nodeshmoved, (l1<l2) ]
                      ++ [ (a,b,()) | (a,l1) <-nodeshmoved, (b,l2) <- nodesg, (l1<l2) ]



-- Creates a subgraph of the join graph of g and h, where g and h are taken as is,
-- and in the connection of both, an edge appears with fixed probability p. The appearences
-- of different edges are (pseudo) stochastically independent
joinGraphRandom :: MonadRandom m => Double -> Gr a () -> Gr a () -> m (Gr a ())
joinGraphRandom p g h = (liftM2 Graph.mkGraph) (return (nodesg ++ nodeshmoved)) ((return edgesg) `rconcat` randedgesprod `rconcat` (return edgeshmoved) )
    where nodesg = Graph.labNodes g 
          hmoved = intMap (+ (snd $ Graph.nodeRange g) ) h -- move nodes from g to start at 1 + (maxnode h)
          nodeshmoved = Graph.labNodes hmoved
          edgesg = Graph.labEdges g
          edgeshmoved = Graph.labEdges hmoved
          randedgesprod = elemWithProb p $ List.nub [ (a,b,()) | (a,_) <-nodesg, (b,_) <- nodeshmoved]

-- Like joinGraphRandom, but an edge (v,w) can only appear if level v < level w
-- It takes a map to get different probabilities for different level combinations
joinLevelGraphRandom :: MonadRandom m => Map.Map (Int,Int) Double -> LevelGraph -> LevelGraph -> m LevelGraph
joinLevelGraphRandom pmap g h = (liftM2 Graph.mkGraph) (return (nodesg ++ nodeshmoved)) ((return edgesg) `rconcat` randedgesprod `rconcat` (return edgeshmoved) )
    where nodesg = Graph.labNodes g 
          hmoved = intMap (+ (snd $ Graph.nodeRange g) ) h -- move nodes from g to start at 1 + (maxnode h)
          nodeshmoved = Graph.labNodes hmoved
          edgesg = Graph.labEdges g
          edgeshmoved = Graph.labEdges hmoved
          newedgeswithps = List.nub $ [ ((a,b,()), (pmap Map.! (l1,l2)) ) | (a,l1) <-nodesg, (b,l2) <- nodeshmoved, l1<l2]
                           ++ [ ((a,b,()), (pmap Map.! (l1,l2)) ) | (a,l1) <-nodeshmoved, (b,l2) <- nodesg, l1<l2]
          randedgesprod = elemWithProbMap newedgeswithps 




genRandomCodeGraph :: MonadRandom m => Map.Map (Int,Int) Double -> Double -> [Int] -> m CodeGraph
genRandomCodeGraph probMap dataSourceProb edgesPerLevel = 
  let
      levelGraphList = zipWith trivialLevelGraph [1..] edgesPerLevel
      levelGraph = Control.Monad.foldM (joinLevelGraphRandom probMap) (trivialLevelGraph 0 0) levelGraphList
  in 
    levelGraph >>= (makeRandomCodeGraph dataSourceProb)


------------------------------------------------------------
-- Examples
------------------------------------------------------------

manualExample :: LevelGraph
manualExample = Graph.mkGraph [(1,1),(2,1),(3,2),(4,2),(5,2)] [(1,3,()),(1,4,()),(1,5,()),(2,3,()),(2,4,()),(2,5,())]

someExample :: CodeGraph
someExample = makeCodeGraph DataSource $ joinLevelGraph (trivialLevelGraph 2 2) (trivialLevelGraph 3 2) 

exampleMap :: Map.Map (Int,Int) Double
--exampleMap = Map.fromList [ ((1,2),0.5), ((1,3),0.2), ((1,4),0.05) ]
exampleMap = Map.fromList [ ((a,b), (1 / 2^(b-a))) | a<- [1..10], b<-[1..10], a<b]

randomExample :: MonadRandom m => m LevelGraph
randomExample = joinLevelGraphRandom exampleMap (trivialLevelGraph 1 2) (trivialLevelGraph 2 3) 


------------------------------------------------------------
-- Test Area
------------------------------------------------------------


kahnTopSort :: Gr a b -> [Graph.LNode a]
kahnTopSort graph = reverse $ helperKahn [] [ (node,label) | (node,label) <- allnodes, (null $ Graph.suc graph node) ]
  where
    allnodes = Graph.labNodes graph
    helperKahn l [] = l
    helperKahn l (s:ss)  = 
        helperKahn (s:l) ss

dfs :: Graph.Node -> Gr n e -> [Graph.Node]
dfs start graph = go [start] graph
  where go [] _                           = []
        go _ g | Graph.isEmpty g          = []
        go (n:ns) (Graph.match n -> (Just c, g)) =
          n : go (Graph.neighbors' c ++ ns) g
        go (_:ns) g                       = go ns g


-- codeGraphNodetoLisp :: Graph.LNode (Level,ComputationType) -> String
-- codeGraphNodetoLisp (a,b,(level,ctype)) = case ctype of DataSource -> "foo"
--                                                         OtherComputation -> "bar"

-- toLispCode :: CodeGraph -> String
-- toLispCode gr = "foo"


-- ----------------
--      TO-DO
-- ----------------
-- Code Generation:
--   - Topological sort with levels: get a list of variables and their order 
--     (maybe as a LevelGraph/CodeGraph specific top.sort algorithm)
--   - Convert topologically-sorted list of nodes using graph (for edges) 
--     into lisp code using the same concept as in the example with nested lets
--   - Repeat process for generating Haskell (Haxl) code (start with example)
--
-- Random staistics:
--   - Add seeds for reproducibility
--   - Create some larger test-suite generating functions
--
--     OPTIONAL:
--
--   - Integrate statistics in the randomness: Averages and deviations
--   - Create several generating functions in the spirit of genRandomCodeGraph
--     which take statistics and not fixed numbers.
--   - Think about the probabilities for the full graphs which are implied by
--     the way the probabilities are currently done
