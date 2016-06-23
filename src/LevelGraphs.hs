{-# LANGUAGE ViewPatterns  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}
-- | Code for generating random level-graphs, a sort of generalization of trees
--   and generating lisp/haskell code following the structure of the graphs.
--   This is part of a project for efficient IO batching using 
--   Ohua (https://bitbucket.org/sertel/ohua), and compare it with Muse and Haxl.
-- 
--
--
--         CCCCCCCCCCCCC        CCCCCCCCCCCCC        CCCCCCCCCCCCC
--      CCC::::::::::::C     CCC::::::::::::C     CCC::::::::::::C
--    CC:::::::::::::::C   CC:::::::::::::::C   CC:::::::::::::::C
--   C:::::CCCCCCCC::::C  C:::::CCCCCCCC::::C  C:::::CCCCCCCC::::C
--  C:::::C       CCCCCC C:::::C       CCCCCC C:::::C       CCCCCC
-- C:::::C              C:::::C              C:::::C              
-- C:::::C              C:::::C              C:::::C              
-- C:::::C              C:::::C              C:::::C              
-- C:::::C              C:::::C              C:::::C              
-- C:::::C              C:::::C              C:::::C              
-- C:::::C              C:::::C              C:::::C              
--  C:::::C       CCCCCC C:::::C       CCCCCC C:::::C       CCCCCC
--   C:::::CCCCCCCC::::C  C:::::CCCCCCCC::::C  C:::::CCCCCCCC::::C
--    CC:::::::::::::::C   CC:::::::::::::::C   CC:::::::::::::::C
--      CCC::::::::::::C     CCC::::::::::::C     CCC::::::::::::C
--         CCCCCCCCCCCCC        CCCCCCCCCCCCC        CCCCCCCCCCCCC
--                                                              
--   ------------- Chair for Compiler Construction ---------------
--   ---------------------- TU Dresden --------------------------
--
--   Author: Andres Goens
--   andres.goens@tu-dresden.de

module LevelGraphs (CodeGraph, LevelGraph, CodeSubGraphs, NestedCodeGraph,
                    makeCodeGraphTimed,
                    toCodeWrapped, makeCodeGraphRandomlyTimed,
                    makeNestedCodeGraphRandomlyTimed,
                    genRandomCodeGraph, setSeed, nullCodeGraph,
                    joinGraphRandom, joinLevelGraphRandom, joinLevelGraph, joinGraph,
                    graph2LevelGraph, makeCodeGraph, fullGraph, concatenateTests,
                    levelsLGraph, levelsCGraph, genRandomCodeGraphBigDS,
                    nullGraph, nullLevelGraph, makeCondCGWithProb, cgGetSubFunctions, 
                    listTests) where

import           Control.Monad        (foldM,liftM,liftM2,mapM,guard,MonadPlus)
import           Control.Monad.Random (MonadRandom,fromList, getRandomR)
import           Data.Maybe           (fromMaybe)
import           Data.Tuple.Sequence         

import qualified System.Random        (mkStdGen, setStdGen)

import qualified Data.Graph.Inductive as Graph
import           Data.Graph.Inductive (Gr)
import qualified Data.List            as List
import qualified Data.Map.Strict      as Map
import qualified Data.Tuple           as Tuple 
--import           Data.Typeable()



data CondBranch = CondBranch Graph.Node | CondNil deriving (Show, Eq)

newtype Depth = Depth Int deriving (Show, Eq, Ord) -- depth remaning (0 means can't spawn a function there)

data ComputationType = DataSource | SideEffect | OtherComputation | 
                       Function | Map |
                       Conditional CondBranch CondBranch CondBranch |
                       SlowDataSource deriving (Show, Eq) --conditional: condition true-branch false-branch

data Statistics = Statistics (Int, Double) deriving (Show, Eq) -- Mu, Sigma

type Level = Int

newtype CodeGraphNodeLabel = CodeGraphNodeLabel (Level,ComputationType, Maybe Int) deriving (Show,Eq)

type LevelGraph = Data.Graph.Inductive.Gr Level () -- There can only be edges (a,b) if level a < level b
type CodeGraph = Gr CodeGraphNodeLabel ()

type CodeSubGraphs = [(CodeGraph, String)]
type NestedCodeGraph = (CodeGraph, CodeSubGraphs) 

instance Ord CodeGraphNodeLabel where
    (<=) (CodeGraphNodeLabel (lvl,_,_)) (CodeGraphNodeLabel (lvl',_,_)) = lvl <= lvl'

-- instance Eq CodeGraphNodeLabel where
--     (==) (CodeGraphNodeLabel (lvl,ctype)) (CodeGraphNodeLabel (lvl',ctype')) = lvl == lvl' && ctype == ctype'
------------------------------------------------------------
-- General helper functions
------------------------------------------------------------

--uses the fact that Graph.Node = Int
intMapNode :: (Int -> Int) -> Graph.Context a b -> Graph.Context a b
intMapNode f (p,n,l,s) = (f' p, f n, l, f' s)
                     where f' context = List.map (\(b,node) -> (b,f node)) context


intMap :: (Int -> Int) -> Gr a b -> Gr a b
intMap f gr = Graph.buildGr $ List.map (intMapNode f) $ Graph.ufold (:) [] gr

minLevel :: LevelGraph -> Level
minLevel = minimum . (map snd) . Graph.labNodes

getLevelCGN :: (Int,CodeGraphNodeLabel) -> Level
getLevelCGN = ((\(CodeGraphNodeLabel (lvl,_,_)) -> lvl ) . snd)

minLevelCG :: CodeGraph -> Level
minLevelCG = minimum . (map getLevelCGN) . Graph.labNodes

-- This works only for level graphs from the defining property for edges
lGraphTopSort :: LevelGraph -> [Graph.LNode Level]
lGraphTopSort = (map Tuple.swap) . List.sort . (map Tuple.swap) . Graph.labNodes

lGraphLevelSort :: LevelGraph -> [[Graph.LNode Level]]
lGraphLevelSort graph = [ subList l | l <- levels ]
  where 
    topSort = lGraphTopSort graph
    levels = List.nub $ map snd topSort
    subList l = [ (node,l) | (node,l') <- topSort, l'==l]

cGraphTopSort :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel]
cGraphTopSort = (map Tuple.swap) . List.sort . (map Tuple.swap) . Graph.labNodes

cGraphLevelSort :: CodeGraph -> [[Graph.LNode CodeGraphNodeLabel]]
cGraphLevelSort graph = [ subList l | l <- levels ]
  where 
    topSort = cGraphTopSort graph
    levels = List.nub $ map getLevelCGN topSort
    subList l = [ (node,CodeGraphNodeLabel (l',ctype,time)) |  (node,CodeGraphNodeLabel (l',ctype,time)) <- topSort, l'==l]

levelsLGraph :: LevelGraph -> Int
levelsLGraph  = length . List.nub . (map snd) . Graph.labNodes

levelsCGraph :: CodeGraph -> Int
levelsCGraph  = length . List.nub . (map getLevelCGN) . Graph.labNodes

subGraphFrom :: Gr a b -> Graph.Node -> Gr a b
subGraphFrom graph start = Graph.subgraph sucnodes graph 
    where 
      sucfn = Graph.suc graph 
      getSucs [] = []
      getSucs nodes =  nodes ++ getSucs (concat $ map sucfn nodes)
      sucnodes = getSucs [start]

cgGetSubFunctions :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel]
cgGetSubFunctions graph = Graph.labNodes $ Graph.labnfilter isFunctionNode graph 
    where
      isFunctionNode :: Graph.LNode CodeGraphNodeLabel -> Bool
      isFunctionNode (_,CodeGraphNodeLabel (_,Function,_)) = True
      isFunctionNode (_,CodeGraphNodeLabel (_,Map,_)) = True
      isFunctionNode _ = False
                         
graphGetLeaves ::  Gr a b -> [Graph.LNode a]
graphGetLeaves gr = Graph.labNodes $ Graph.nfilter (null . Graph.suc gr) gr


------------------------------------------------------------
-- Random-monad helper functions
------------------------------------------------------------

setSeed :: Int -> IO ()
setSeed = System.Random.setStdGen . System.Random.mkStdGen 

rconcat :: MonadRandom m => m [a] -> m [a] -> m [a]
rconcat = liftM2 (++)

rguard ::(MonadRandom m, MonadPlus n) => m Bool -> m (n ())
rguard = (liftM guard)


--the following 4 functions adapted from http://www.rosettacode.org/wiki/Knuth_shuffle#Haskell
mkRandListKnuth :: MonadRandom m => Int -> m [Int]
mkRandListKnuth =  mapM (getRandomR . (,) 0) . enumFromTo 1 . pred

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i c l = let (a,b) = splitAt i l in a++c:(drop 1 b)
 
transposition :: (Int, Int) -> [a] -> [a]
transposition (i,j) list | i==j = list
                         | otherwise = replaceAt j (list!!i) $ replaceAt i (list!!j) list
knuthShuffle :: MonadRandom m => [a] -> m [a]
knuthShuffle xs =
  liftM (foldr transposition xs. zip [1..]) (mkRandListKnuth (length xs))

trueWithProb :: MonadRandom m => Double -> m Bool
trueWithProb p = let p' = toRational p in Control.Monad.Random.fromList [ (True, p'), (False, (1 - p')) ]

dsWithProb :: MonadRandom m => Double -> m ComputationType
dsWithProb p = let p' = toRational p in Control.Monad.Random.fromList [ (DataSource, p'), (OtherComputation, (1 - p')) ]

ctWithProb :: MonadRandom m => [Double] -> m ComputationType
ctWithProb ps = 
    let
        ps' = map toRational ps 
    in Control.Monad.Random.fromList $ zip [DataSource, SideEffect, Function, Map, OtherComputation] (ps' ++ [1 - (sum ps')])

condWithProb :: MonadRandom m => Double -> Gr a () -> Graph.Context CodeGraphNodeLabel b -> m (Graph.Context CodeGraphNodeLabel b)
condWithProb p graph oldctx@(pre,node,CodeGraphNodeLabel (lvl,_,_),children) = 
    if (length children > 3 || length children < 3 || (elem [] $ map (Graph.suc graph) (Graph.suc graph node) )) then return oldctx -- until we figure out a better way
    else newctx >>= (\x -> return [(x,p'), (oldctx,1-p') ]) >>= Control.Monad.Random.fromList 
         where p' = toRational p
               randomBranches :: MonadRandom m => m [Maybe Graph.Node]
               randomBranches = knuthShuffle $ take 3 $ (map Just $ map snd children) ++ [Nothing,Nothing,Nothing]
               randomConditional = liftM mkConditional $ randomBranches
               newctx = liftM  (\x -> (pre,node,CodeGraphNodeLabel (lvl, x,Nothing),children)) randomConditional


emptyWithProb :: MonadRandom m => Double -> [a] -> m [a]
emptyWithProb p list = let p' = toRational p in Control.Monad.Random.fromList [ (list, p'), ([], (1 - p')) ]

-- caution: because I'm lazy and don't want to properly use tranformers, this is probably inefficient
elemWithProb :: MonadRandom m => Double -> [a] -> m [a] 
elemWithProb _ [] = return []
elemWithProb p (e:es) =  liftM2 (++) (emptyWithProb p [e]) (elemWithProb p es)

elemWithProbList :: MonadRandom m => [(a,Double)] -> m [a] 
elemWithProbList [] = return []
elemWithProbList ((e,p):es) =  liftM2 (++) (emptyWithProb p [e]) (elemWithProbList es)


------------------------------------------------------------
-- Conversion Functions
------------------------------------------------------------
mkConditional :: [Maybe Graph.Node] -> ComputationType
mkConditional (cond:true:false:_) = Conditional (mkCNode cond) (mkCNode true) (mkCNode false)
    where mkCNode Nothing = CondNil
          mkCNode (Just n) = CondBranch n
mkConditional incompleteList = mkConditional (incompleteList ++ [Nothing])

addLevelContext :: Level -> Graph.Context () b -> Graph.Context Level b
addLevelContext level (pre,node,(),after) = (pre,node,level,after)

addCodeContext :: ComputationType -> Maybe Int -> Graph.Context Level b -> Graph.Context CodeGraphNodeLabel b
addCodeContext ctype time (pre,node,lvl,after) = (pre,node,CodeGraphNodeLabel (lvl,ctype,time),after)

graph2LevelGraph ::  Level -> Gr () () -> LevelGraph
graph2LevelGraph level gr = Graph.buildGr (List.map (addLevelContext level) unfolded)
    where unfolded = Graph.ufold (:) [] gr

qlEdge2Edge :: Graph.LEdge () -> Graph.Edge
qlEdge2Edge (a,b,()) = (a,b)

makeCodeGraph :: ComputationType -> LevelGraph -> CodeGraph
makeCodeGraph ctype = Graph.nmap (\l -> CodeGraphNodeLabel (l,ctype,Nothing) ) 

-- Makes a level graph into a code graph with a probability p for being a DataSource for every node
makeRandomCodeGraph :: MonadRandom m => [Double] -> LevelGraph -> m CodeGraph 
makeRandomCodeGraph probsCT gr = liftM Graph.buildGr transformed
    where 
      unfolded = Graph.ufold (:) [] gr
      transformed = flip Control.Monad.mapM unfolded $ \ctx -> do ctype <- ctWithProb probsCT
                                                                  return $ addCodeContext ctype Nothing ctx

makeCondCGWithProb :: MonadRandom m => Double -> CodeGraph -> m CodeGraph
makeCondCGWithProb p gr = 
  liftM Graph.buildGr transformed
    where
      unfolded = Graph.ufold (:) [] gr
      makeCond ::  MonadRandom m => Graph.Context CodeGraphNodeLabel b -> m (Graph.Context CodeGraphNodeLabel b) 
      makeCond = \x@(_,node,_,_) -> if (null $ Graph.suc gr node) then return x else condWithProb p gr x
      transformed = flip Control.Monad.mapM unfolded $ makeCond
      

makeGraphRooted ::  a -> Gr a () -> Gr a ()
makeGraphRooted rootlabel graph = Graph.mkGraph (oldNodes ++ [(unocupied,rootlabel)]) (oldEdges ++ newEdges) 
--makeGraphRooted _ graph  = newEdges
    where
      unocupied = (+1) $ snd $ Graph.nodeRange graph
      oldEdges = Graph.labEdges graph
      oldNodes = Graph.labNodes graph
      oldNodes' = map fst $ Graph.labNodes graph
      newEdges = [ (unocupied,node,()) | node <- oldNodes', (null $ Graph.pre graph node) ]


makeGraphUnbalancedBigTree ::  CodeGraph -> CodeGraph
makeGraphUnbalancedBigTree graph = Graph.mkGraph (oldNodes ++ [(unocupied,rootnodelabel)] ++ [bigsource]) (oldEdges ++ newEdges) 
    where
      unocupied = (+1) $ snd $ Graph.nodeRange graph
      startlvl =  (pred . minLevelCG) graph
      rootnodelabel = CodeGraphNodeLabel (startlvl,OtherComputation,Nothing)
      bigsource = (succ unocupied, CodeGraphNodeLabel (succ startlvl, SlowDataSource,Nothing))
      oldEdges = Graph.labEdges graph
      oldNodes = Graph.labNodes graph
      oldNodes' = map fst $ Graph.labNodes graph
      newEdges' = [ (unocupied,node,()) | node <- oldNodes', (null $ Graph.pre graph node) ]
      newEdges = (unocupied,unocupied+1,()):newEdges'


makeCGNodeTimed :: Int -> CodeGraphNodeLabel -> CodeGraphNodeLabel
makeCGNodeTimed n = (\(CodeGraphNodeLabel (l,ctype,_)) -> CodeGraphNodeLabel (l,ctype,Just n) ) 

makeCodeGraphTimed :: Int -> CodeGraph  -> CodeGraph
makeCodeGraphTimed = Graph.nmap . makeCGNodeTimed 


------------------------------------------------------------
-- Basic graph families
------------------------------------------------------------

--uses the fact that Graph.Node = Int
fullGraph :: Int -> Gr () ()
fullGraph n = Graph.mkUGraph vs (concat $ map (\x -> [(x,a) | a <- [(x+1)..n] ] ++  [(a,x) | a <- [(x+1)..n] ] ) vs)
    where vs = [1..n]

nullGraph :: Int -> Gr () ()
nullGraph n = Graph.mkUGraph [1..n] []

nullLevelGraph :: Level -> Int -> LevelGraph
nullLevelGraph level n = Graph.buildGr $ List.map (\x -> ([],x,level,[])) [1..n]

nullCodeGraph :: ComputationType -> Level -> Int -> CodeGraph
nullCodeGraph ctype lvl n  = (makeCodeGraph ctype) $ nullLevelGraph lvl n

------------------------------------------------------------
-- Graph generating functions from graphs
------------------------------------------------------------

joinGraph :: Gr a () -> Gr a () -> Gr a ()
joinGraph g h = Graph.mkGraph (gNodes ++ hNodes') 
                (gEdges ++ prodEdges ++ hEdges')
  where gNodes = Graph.labNodes g
        newInt = if Graph.isEmpty g then 1 else (snd $ Graph.nodeRange g)
        h' = intMap (+ newInt ) h -- move nodes from g to start at 1 + (maxnode h)
        hNodes' = Graph.labNodes h'
        gEdges = Graph.labEdges g
        hEdges' = Graph.labEdges h'
        prodEdges = List.nub $ 
                    [ (a,b,()) | (a,_) <-gNodes,
                      (b,_) <- hNodes' ]
                    ++ [ (a,b,()) | (a,_) <-hNodes',
                         (b,_) <- gNodes]

joinLevelGraph :: LevelGraph -> LevelGraph -> LevelGraph
joinLevelGraph g h = Graph.mkGraph (gNodes ++ hNodes') (gEdges ++ prodEdges ++ hEdges')
--joinLevelGraph g h = (gEdges , edgesprod , hEdges')
    where gNodes = Graph.labNodes g
          newInt = if Graph.isEmpty g then 1 else (snd $ Graph.nodeRange g)
          h' = intMap (+ newInt ) h -- move nodes from g to start at 1 + (maxnode h)
          hNodes' = Graph.labNodes h'
          gEdges = Graph.labEdges g
          hEdges' = Graph.labEdges h'
          prodEdges = List.nub $ [ (a,b,()) | (a,l1) <-gNodes, (b,l2) <- hNodes', (l1<l2) ]
                      ++ [ (a,b,()) | (a,l1) <-hNodes', (b,l2) <- gNodes, (l1<l2) ]



-- Creates a subgraph of the join graph of g and h, where g and h are taken as is,
-- and in the connection of both, an edge appears with fixed probability p. The appearences
-- of different edges are (pseudo) stochastically independent
joinGraphRandom :: MonadRandom m => Double -> Gr a () -> Gr a () -> m (Gr a ())
joinGraphRandom p g h = (liftM2 Graph.mkGraph) (return (gNodes ++ hNodes')) ((return gEdges) `rconcat` prodEdgesRand `rconcat` (return hEdges') )
    where gNodes = Graph.labNodes g 
          newInt = if Graph.isEmpty g then 1 else (snd $ Graph.nodeRange g)
          h' = intMap (+ newInt ) h -- move nodes from g to start at 1 + (maxnode h)
          hNodes' = Graph.labNodes h'
          gEdges = Graph.labEdges g
          hEdges' = Graph.labEdges h'
          prodEdgesRand = elemWithProb p $ List.nub [ (a,b,()) | (a,_) <-gNodes, (b,_) <- hNodes']

-- Like joinGraphRandom, but an edge (v,w) can only appear if level v < level w
-- It takes a map to get different probabilities for different level combinations
joinLevelGraphRandom :: MonadRandom m => Map.Map (Int,Int) Double -> LevelGraph -> LevelGraph -> m LevelGraph
joinLevelGraphRandom pmap g h = (liftM2 Graph.mkGraph) (return (gNodes ++ hNodes')) ((return gEdges) `rconcat` prodEdgesRand `rconcat` (return hEdges') )
    where gNodes = Graph.labNodes g 
          newInt = if Graph.isEmpty g then 1 else (snd $ Graph.nodeRange g)
          h' = intMap (+ newInt ) h -- move nodes from g to start at 1 + (maxnode h)
          hNodes' = Graph.labNodes h'
          gEdges = Graph.labEdges g
          hEdges' = Graph.labEdges h'
          prodEdgesProb = List.nub $
                          [((a,b,()), (pmap Map.! (l1,l2)))
                           | (a,l1) <-gNodes, 
                           (b,l2) <- hNodes', l1<l2] ++
                          [((a,b,()), (pmap Map.! (l1,l2)))
                           | (a,l1) <-hNodes',
                           (b,l2) <- gNodes, l1<l2]
          prodEdgesRand = elemWithProbList prodEdgesProb




genRandomCodeGraph :: MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> [Int] -> m CodeGraph
genRandomCodeGraph _ cTypeProb [] = makeRandomCodeGraph cTypeProb (nullLevelGraph 1 1)
genRandomCodeGraph probMap cTypeProb edgesPerLevel = 
  let
      levelGraphList = zipWith nullLevelGraph [1..] edgesPerLevel
      levelGraph' = Control.Monad.foldM (joinLevelGraphRandom probMap) (nullLevelGraph 0 0) levelGraphList
      levelGraph = Control.Monad.liftM2 makeGraphRooted (liftM (pred . minLevel) levelGraph') levelGraph'
  in 
    levelGraph >>= (makeRandomCodeGraph cTypeProb)

genRandomCodeGraphBigDS :: MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> [Int] -> m CodeGraph
genRandomCodeGraphBigDS _ cTypeProb [] = makeRandomCodeGraph cTypeProb (nullLevelGraph 1 1)
genRandomCodeGraphBigDS probMap cTypeProb edgesPerLevel = 
  let
      levelGraphList = zipWith nullLevelGraph [1..] edgesPerLevel
      levelGraph = Control.Monad.foldM (joinLevelGraphRandom probMap) (nullLevelGraph 0 0) levelGraphList
      codeGraph' = levelGraph >>= (makeRandomCodeGraph cTypeProb) 
  in liftM makeGraphUnbalancedBigTree codeGraph'

-- makeCodeGraphRandomlyTimed :: MonadRandom m => Int -> CodeGraph  -> m CodeGraph
-- makeCodeGraphRandomlyTimed n graph = liftM2 (flip Graph.nmap) (return graph) <=< makeTimed
--     where 
--         makeTimed :: MonadRandom m => CodeGraphNodeLabel ->  m CodeGraphNodeLabel
--         makeTimed label = do 
--           randn <- getRandomR (1,n)
--           return $ (\(CodeGraphNodeLabel (l,ctype,_)) -> CodeGraphNodeLabel (l,ctype,Just randn)) label

makeCodeGraphRandomlyTimed :: MonadRandom m => Int -> CodeGraph  -> m CodeGraph
makeCodeGraphRandomlyTimed n gr = liftM Graph.buildGr transformed
    where 
      unfolded = Graph.ufold (:) [] gr
      transformed = flip Control.Monad.mapM unfolded $ \ctx -> do newn <- getRandomR (1,n)
                                                                  return $ (\(pre,node,label,after) -> (pre,node, makeCGNodeTimed newn label, after)) ctx


makeNestedCodeGraphRandomlyTimed :: forall m . MonadRandom m => Int -> NestedCodeGraph  -> m NestedCodeGraph
makeNestedCodeGraphRandomlyTimed t (maingraph, subgraphs) = sequenceT ((makeCodeGraphRandomlyTimed t maingraph , mapM makeSubgraphRandomlyTimed subgraphs) :: (m CodeGraph, m CodeSubGraphs))
    where makeSubgraphRandomlyTimed :: (CodeGraph, String) -> m (CodeGraph, String)
          makeSubgraphRandomlyTimed = (\(x,y) -> sequenceT ((makeCodeGraphRandomlyTimed t x, return y) :: (m CodeGraph, m String)))
-- nmapM :: (Graph.DynGraph gr, MonadRandom m) => (a -> m c) -> gr a b -> m (gr c b)
-- nmapM f = gmapM (\(p,v,l,s)->(p,v,f l,s))
--     where 
--       gmapM f = ufold (\c->(f c&)) empty

------------------------------------------------------------
-- Clojure Backends
------------------------------------------------------------

nodeToUniqueNameClojure :: Graph.Node -> String
nodeToUniqueNameClojure  =  (++) "local-" . show 

cgNodeToClojureFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,DataSource,time)) = 
    "(get-data " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " \"service-name\" " ++ (show $ fromMaybe n time)  ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,SlowDataSource,time)) = 
    "(slow-get-data " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " \"service-name\" " ++ (show $ 10000 + fromMaybe n time)  ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,OtherComputation,time)) = 
    "(compute " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " " ++ (show $ fromMaybe n time) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,SideEffect,time)) = 
    "(write-data " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " \"service-name\" " ++ (show $ fromMaybe n time) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,Function,time)) =
    "(fn-" ++ nodeToUniqueNameClojure n ++ (if null children then [] else " ") ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ ")"
cgNodeToClojureFunction _ children (n,CodeGraphNodeLabel (_,Map,time)) =
    "(map fn-" ++ nodeToUniqueNameClojure n ++ " [" ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ "] " ++ ")"
cgNodeToClojureFunction graph _ (_,CodeGraphNodeLabel (_,Conditional (CondBranch cond) trueBranch falseBranch,_)) =
    "(if " ++ nodeToUniqueNameClojure cond ++ " " ++ List.intercalate " " (map maybeNodeToUniqueName [trueBranch,falseBranch] ) ++ ")"
    where maybeNodeToUniqueName CondNil = "nil"
          maybeNodeToUniqueName (CondBranch node) = nodeToUniqueNameHaskell node

cgNodeToClojureAppFunction :: CodeGraph -> [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,DataSource,time)) = 
    "(get-data " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " \"service-name\" " ++ (show $ fromMaybe n time)  ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,SlowDataSource,time)) = 
    "(slow-get-data " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " \"service-name\" " ++ (show $ 10000 + fromMaybe n time)  ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,OtherComputation,time)) = 
    "(<$> compute (return " ++ List.intercalate ") (return " (map nodeToUniqueNameClojure children) ++ (if length children == 0 then " " else ") (return ") ++ (show $ fromMaybe n time) ++ "))"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,SideEffect,time)) = 
    "(write-data " ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ " \"service-name\" " ++ (show $ fromMaybe n time) ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,Function,time)) =
    "(fn-" ++ nodeToUniqueNameClojure n ++ (if null children then [] else " ") ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ ")"
cgNodeToClojureAppFunction _ children (n,CodeGraphNodeLabel (_,Map,time)) =
    "(map fn-" ++ nodeToUniqueNameClojure n ++ " [" ++ List.intercalate " " (map nodeToUniqueNameClojure children) ++ "] " ++ ")"
cgNodeToClojureAppFunction graph _ (_,CodeGraphNodeLabel (_,Conditional (CondBranch cond) trueBranch falseBranch,_)) = 
    "(if " ++ nodeToUniqueNameClojure cond ++ " " ++ List.intercalate " " (map maybeNodeToUniqueName [trueBranch,falseBranch] ) ++ ")"
    where maybeNodeToUniqueName CondNil = "nil"
          maybeNodeToUniqueName (CondBranch node) = nodeToUniqueNameHaskell node


cgNodeToClojureLetDef :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToClojureLetDef toCode graph = (\x -> (nodeToUniqueNameClojure $ fst x) ++ " " ++ (cgNodeToClojureFunction graph (Graph.suc graph $ fst x) x))


toClojureSubFunctions :: (CodeGraph -> String) -> [(CodeGraph, String)] -> String
toClojureSubFunctions _ [] = ""
toClojureSubFunctions toCode subgraphs = List.intercalate "\n" (map (toClojureSubFunction toCode) subgraphs)

toClojureSubFunction :: (CodeGraph -> String) -> (CodeGraph, String) -> String
toClojureSubFunction toCode (graph, name) = 
    let 
        leaves = graphGetLeaves graph
        numLeaves = length leaves
        parameterNames = map (\x -> "parameter-" ++ show x) [1..]
    in "(defn " ++ name ++ " [ " ++ (concat $ take numLeaves parameterNames) ++ "]" ++ "(\n" ++ toCode graph ++ "\n))"


--cgNodeToClojureApplicative :: (CodeGraph -> String) -> CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
--cgNodeToClojureApplicative toCode graph = (\x -> (nodeToUniqueNameClojure $ fst x) ++ " " ++ ((cgNodeToClojureFunction toCode) graph (Graph.suc graph $ fst x) x))

-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

toMuseMonadCode :: CodeGraph -> String
toMuseMonadCode graph = helperToMuseCode nodes ++ "\n"
    where 
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToMuse levelNodes = "mlet [" ++ List.intercalate " " (map (cgNodeToClojureLetDef toMuseMonadCode graph) levelNodes) ++ "]"
      helperToMuseCode [] = ""
      helperToMuseCode [[lastLvlNode]] = cgNodeToClojureFunction graph (Graph.suc graph $ fst lastLvlNode) lastLvlNode ++ "\n"
      helperToMuseCode (lvl:lvls) = "(" ++ (levelToMuse lvl) ++ "\n" ++ (helperToMuseCode lvls) ++ ")"


cgNodesToMuseApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToMuseApplicative graph [] = ""
cgNodesToMuseApplicative graph [node@(nd, _)] = "" ++ (cgNodeToClojureAppFunction graph (Graph.suc graph $ nd) node) ++ ""
cgNodesToMuseApplicative graph nodes = "(<$> clojure.core/vector "  
                                ++  (List.intercalate " " (map (\x -> toFun x $ Graph.suc graph $ fst x) nodes)) ++ ")"
    where 
      toFun node = flip (cgNodeToClojureAppFunction graph) node


toMuseMonadCodeWrapped :: String -> NestedCodeGraph -> String
toMuseMonadCodeWrapped testname (graph, subgraphs) = (toClojureSubFunctions toMuseMonadCode subgraphs)
                                                     ++ "(defn " ++ testname ++ " []\n(run!! \n" ++ toMuseMonadCode graph ++ "))"

toMuseAppCode :: CodeGraph -> String
toMuseAppCode graph =  helperToMuseApp nodes
    where 
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levels = length nodes
      levelToDoApp [levelNode] = (nodeToUniqueNameClojure . fst) levelNode ++ " " ++ cgNodesToMuseApplicative graph [levelNode]
      levelToDoApp levelNodes = "[" ++ List.intercalate " " (map (nodeToUniqueNameClojure . fst) levelNodes)
                                ++ "] " ++ cgNodesToMuseApplicative graph levelNodes
      helperToMuseApp [] = ""
      helperToMuseApp [[lastnode]] = (if levels == 1 then "" else "]") ++ cgNodeToClojureAppFunction graph [] lastnode
      helperToMuseApp (lvl:lvls) = (levelToDoApp lvl) ++ "\n" ++ (helperToMuseApp lvls) ++ ""

toMuseAppCodeWrapped :: String -> NestedCodeGraph -> String
toMuseAppCodeWrapped testname (graph, subgraphs) = 
    let maingraph = if (levelsCGraph graph == 1)
                    then "(defn " ++ testname ++ " [] (run!! \n" ++ toMuseAppCode graph ++ "))\n"
                    else "(defn " ++ testname ++ " [] (run!! \n (mlet [ " ++ toMuseAppCode graph ++ ")))\n"
        subs = toClojureSubFunctions toMuseAppCode subgraphs
    in subs ++ "\n" ++ maingraph

cgNodesToOhuaApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToOhuaApplicative graph [] = ""
cgNodesToOhuaApplicative graph [node@(nd, _)] = "" ++ (cgNodeToClojureFunction graph (Graph.suc graph $ nd) node) ++ ""
cgNodesToOhuaApplicative graph nodes = "(vector " ++ List.intercalate " " (map (\x -> toFun x $ Graph.suc graph $ fst x) nodes) ++ ")"
    where 
      toFun node = flip (cgNodeToClojureFunction graph) node

toOhuaAppCode :: CodeGraph -> String
toOhuaAppCode graph =  helperToOhuaApp nodes
    where 
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levels = length nodes
      levelToDoApp [levelNode] = (nodeToUniqueNameClojure . fst) levelNode ++ " " ++ cgNodesToOhuaApplicative graph [levelNode]
      levelToDoApp levelNodes = "[" ++ List.intercalate " " (map (nodeToUniqueNameClojure . fst) levelNodes)
                                ++ "] " ++ cgNodesToOhuaApplicative graph levelNodes
      helperToOhuaApp [] = ""
      helperToOhuaApp [[lastnode]] = (if levels == 1 then "" else "]") ++ cgNodeToClojureFunction graph [] lastnode
      helperToOhuaApp (lvl:lvls) = (levelToDoApp lvl) ++ "\n" ++ (helperToOhuaApp lvls) ++ ""

toOhuaAppCodeWrapped :: String -> NestedCodeGraph -> String
toOhuaAppCodeWrapped testname (graph, subgraphs) = 
    let maingraph = if (levelsCGraph graph == 1)
                    then "(defn " ++ testname ++ " [] (ohua \n" ++ toOhuaAppCode graph ++ "))\n"
                    else "(defn " ++ testname ++ " [] (ohua \n (let [ " ++ toOhuaAppCode graph ++ ")))\n"
        subs = toClojureSubFunctions toOhuaAppCode subgraphs
    in subs ++ "\n" ++ maingraph

-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

toOhuaCode :: CodeGraph -> String
toOhuaCode graph = helperToOhuaCode nodes ++ "\n"
    where 
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToOhua levelNodes = "let [" ++ List.intercalate " " (map (cgNodeToClojureLetDef toOhuaCode graph) levelNodes) ++ "]"
      helperToOhuaCode [] = ""
      helperToOhuaCode [[lastLvlNode]] = cgNodeToClojureFunction graph (Graph.suc graph $ fst lastLvlNode) lastLvlNode ++ "\n"
      helperToOhuaCode (lvl:lvls) = "(" ++ (levelToOhua lvl) ++ "\n" ++ (helperToOhuaCode lvls) ++ ")"

toOhuaCodeWrapped :: String -> NestedCodeGraph -> String
toOhuaCodeWrapped testname (graph, subgraphs) = toClojureSubFunctions toOhuaCode subgraphs
                                   ++ "\n" ++ "(defn " ++ testname ++ " []\n(ohua\n" ++ toOhuaCode graph ++ "))"


------------------------------------------------------------
-- Haskell Backend
------------------------------------------------------------

nodeToUniqueNameHaskell :: Graph.Node -> String
nodeToUniqueNameHaskell  =  (++) "local" . show 

helperNodeToHaskellFunction :: [Graph.Node] -> String
helperNodeToHaskellFunction children = listStart ++ List.intercalate ", " (map nodeToUniqueNameHaskell children) 
    where listStart = if null children then "" else ", "

cgNodeToHaskellFunction :: [Graph.Node] -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,DataSource,time)) = 
    "getData \"service-name\" [" ++ (show $ fromMaybe n time) ++ helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,SlowDataSource, time)) = 
    "slowGetData \"service-name\" [" ++ (show $ 10000 + fromMaybe n time) ++ helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,OtherComputation,time)) = 
    "compute [" ++ (show $ fromMaybe n time) ++  helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,Function,time)) = 
    "fun'" ++ nodeToUniqueNameHaskell n ++ " "  ++ List.intercalate " " (map nodeToUniqueNameHaskell children)
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,Map,time)) = 
    "map fun'" ++ nodeToUniqueNameHaskell n ++ " [" ++ (show $ fromMaybe n time) ++ helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction children (n,CodeGraphNodeLabel (_,SideEffect,time)) = 
    "writeData \"service-name\" [" ++ (show $ fromMaybe n time) ++  helperNodeToHaskellFunction children ++ "]"
cgNodeToHaskellFunction _ (_,CodeGraphNodeLabel (_,Conditional cond trueBranch falseBranch,_)) = 
    "if " ++ (maybeNodeToUniqueName cond) ++ " then " ++ (maybeNodeToUniqueName trueBranch) ++ " else " ++  (maybeNodeToUniqueName falseBranch)
           where maybeNodeToUniqueName CondNil = "nil"
                 maybeNodeToUniqueName (CondBranch node) = nodeToUniqueNameHaskell node
                 

cgNodeToHaskellDoBind:: CodeGraph -> Graph.LNode CodeGraphNodeLabel -> String
cgNodeToHaskellDoBind graph = (\x -> (nodeToUniqueNameHaskell $ fst x) ++ " <- " ++ (cgNodeToHaskellFunction (Graph.suc graph $ fst x) x))

-- assumes the level graph is connected!
-- assumes the lowest level has exactly one element!
-- (otherwise there is no call in the end)

cgNodesToHaxlApplicative :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel] -> String
cgNodesToHaxlApplicative graph [] = ""
cgNodesToHaxlApplicative graph [node] = cgNodeToHaskellFunction (Graph.suc graph $ fst node) node 
cgNodesToHaxlApplicative graph nodes = "(" ++ (flip replicate ',' $ pred $ length nodes) ++ ") <$> "  
                                ++  (List.intercalate " <*> " (map (\x -> flip cgNodeToHaskellFunction x $ Graph.suc graph $ fst x) nodes)) 

toHaskellDoCode :: CodeGraph -> String
toHaskellDoCode graph = helperToHaskellDoCode nodes ++ "\n"
    where 
      nodes = reverse $ cGraphTopSort graph --bottom up
      trSpace = "  "
      helperToHaskellDoCode ns = (concat $ map (\x -> trSpace ++ cgNodeToHaskellDoBind graph x ++ "\n") ns) ++ trSpace ++ "return " ++ nodeToUniqueNameHaskell (fst $ last ns) ++ "\n"

-- Here: add recursive call to generate functions in Function and Map nodes
toHaskellDoCodeWrapped :: String -> NestedCodeGraph -> String
toHaskellDoCodeWrapped testname (graph, subgraphs) = testname ++ " :: Env u -> IO Int\n" ++
                                                     testname ++ " myEnv =\n" ++
                                                     "    runHaxl myEnv $ do\n" ++
                                                     toHaskellDoCode graph ++ "\n"
-- implement haskell subgraphs

toHaskellDoAppCode :: CodeGraph -> String
toHaskellDoAppCode graph = helperToDoApp nodes ++ "\n"
    where 
      nodes = reverse $ cGraphLevelSort graph --bottom up
      levelToDoApp [levelNode] = (nodeToUniqueNameHaskell . fst) levelNode 
                                ++ " <- " ++ cgNodesToHaxlApplicative graph [levelNode]
      levelToDoApp levelNodes = "(" ++ List.intercalate ", " (map (nodeToUniqueNameHaskell . fst) levelNodes)
                                ++ ") <- " ++ cgNodesToHaxlApplicative graph levelNodes
      helperToDoApp [] = ""
      helperToDoApp [[lastLvlNode]] = "        " ++ cgNodesToHaxlApplicative graph [lastLvlNode] ++ "\n"
      helperToDoApp (lvl:lvls) = "        " ++ (levelToDoApp lvl) ++ "\n" ++ (helperToDoApp lvls)

-- Here: add recursive call to generate functions in Function and Map nodes
toHaskellDoAppCodeWrapped :: String -> NestedCodeGraph -> String
toHaskellDoAppCodeWrapped testname (graph, subgraphs) = 
    testname ++ " :: Env u -> IO Int\n" ++
    testname ++ " myEnv =\n" ++ "    runHaxl myEnv $ do\n" ++
    toHaskellDoAppCode graph ++ "\n"




------------------------------------------------------------
-- Graph Backend
------------------------------------------------------------

toGraphCodeWrapped :: String -> NestedCodeGraph -> String
toGraphCodeWrapped name (graph, subgraphs) = 
    "Graph-" ++ name ++ "\n" ++ Graph.prettify graph ++ "\n" ++
    (concat $ flip map subgraphs (\(subgraph,_) -> "Subgraph-" ++ name ++ "\n" ++ Graph.prettify subgraph ++ "\n"))
           
------------------------------------------------------------
-- General Backend
------------------------------------------------------------

-- Improve: replace "string" with a good language type
toCodeWrapped :: String -> String -> NestedCodeGraph -> String
toCodeWrapped lang =
    case lang of
      "HaskellDo" -> toHaskellDoCodeWrapped
      "HaskellDoApp" -> toHaskellDoAppCodeWrapped
      "Ohua" ->  toOhuaCodeWrapped
      "OhuaApp" ->  toOhuaAppCodeWrapped
      "Graph" -> toGraphCodeWrapped
      "MuseMonad" -> toMuseMonadCodeWrapped
      "MuseApp" -> toMuseAppCodeWrapped
      _ -> (\_ _ -> "Unexpected language case error")

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
-- Benchmark Code
------------------------------------------------------------
concatenateTests ::  (String -> NestedCodeGraph -> String) -> [ NestedCodeGraph ] -> String
concatenateTests toCodeWrapped randomGraphs = singleString
    where
      randomGraphsNumbered = zip [0..] randomGraphs
      totallevels = maximum $ map (levelsCGraph . fst) randomGraphs 
      strings = map (\(x,y) -> toCodeWrapped ("run_test_level" ++ (show $ levelsCGraph $ fst y) ++ "_" ++ show (x `quot` totallevels)) y) randomGraphsNumbered
      singleString = List.intercalate "\n" strings


listTests ::  [ NestedCodeGraph ] -> String
listTests randomGraphsTup = singleString
    where
      randomGraphs = map fst randomGraphsTup
      randomGraphsNumbered = zip [0..] randomGraphs
      totallevels = maximum $ map levelsCGraph randomGraphs 
      makeNameString (x,y) = "(run_test_level" ++ curLvlStr ++ "_" ++ curInstanceStr ++ ", " ++ curLvlStr ++ ", " ++ curInstanceStr ++ ")"
          where curLvlStr = (show $ levelsCGraph y) 
                curInstanceStr =  show (x `quot` totallevels)
      strings = map makeNameString randomGraphsNumbered
      singleString = "[" ++ (List.intercalate ", " strings) ++ "]"


------------------------------------------------------------
-- Test Area
------------------------------------------------------------

--main = do
--  str <- Control.Monad.Random.evalRandIO someExampleStringsVarLength
--  putStrLn str

-- ----------------
--      TO-DO
-- ----------------
--
--     OPTIONAL:
--
--   - Integrate statistics in the randomness: Averages and deviations
--   - Create several generating functions in the spirit of genRandomCodeGraph
--     which take statistics and not fixed numbers.
--   - Think about the probabilities for the full graphs which are implied by
--     the way the probabilities are currently done


 
