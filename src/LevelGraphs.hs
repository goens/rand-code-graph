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

module LevelGraphs  where

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
import Control.Arrow (second)
import Data.Function (on)
--import           Data.Typeable()

data CondBranch = CondBranch Graph.Node | CondNil deriving (Show, Eq)

newtype Depth = Depth Int deriving (Show, Eq, Ord) -- depth remaning (0 means can't spawn a function there)

data ComputationType
    = DataSource
    | SideEffect
    | OtherComputation
    | Function
    | Map
    | NamedFunction String
    | Conditional CondBranch CondBranch CondBranch
    | SlowDataSource
    deriving (Show, Eq) --conditional: condition true-branch false-branch

data Statistics = Statistics (Int, Double) deriving (Show, Eq) -- Mu, Sigma

type Level = Int

data CodeGraphNodeLabel = CodeGraphNodeLabel
    { level :: Level
    , computationType :: ComputationType
    , timeout :: Maybe Int
    } deriving (Show,Eq)

type LevelGraph = Data.Graph.Inductive.Gr Level () -- There can only be edges (a,b) if level a < level b
type CodeGraph = Gr CodeGraphNodeLabel ()

type CodeSubGraphs = [(CodeGraph, String)]
type NestedCodeGraph = (CodeGraph, CodeSubGraphs)

instance Ord CodeGraphNodeLabel where
    compare = compare `on` level

-- instance Eq CodeGraphNodeLabel where
--     (==) (CodeGraphNodeLabel (lvl,ctype)) (CodeGraphNodeLabel (lvl',ctype')) = lvl == lvl' && ctype == ctype'
------------------------------------------------------------
-- General helper functions
------------------------------------------------------------

--uses the fact that Graph.Node = Int
intMapNode :: (Int -> Int) -> Graph.Context a b -> Graph.Context a b
intMapNode f (p,n,l,s) = (f' p, f n, l, f' s)
                     where f' context = fmap (second f) context


intMap :: (Int -> Int) -> Gr a b -> Gr a b
intMap f gr = Graph.buildGr $ List.map (intMapNode f) $ Graph.ufold (:) [] gr

minLevel :: LevelGraph -> Level
minLevel = minimum . (map snd) . Graph.labNodes

maxLevel :: LevelGraph -> Level
maxLevel = maximum . (map snd) . Graph.labNodes

getLevelCGN :: (Int,CodeGraphNodeLabel) -> Level
getLevelCGN = level . snd

minLevelCG :: CodeGraph -> Level
minLevelCG = minimum . (map getLevelCGN) . Graph.labNodes

maxLevelCG :: CodeGraph -> Level
maxLevelCG = maximum . (map getLevelCGN) . Graph.labNodes

-- This works only for level graphs from the defining property for edges
lGraphTopSort :: LevelGraph -> [Graph.LNode Level]
lGraphTopSort = (map Tuple.swap) . List.sort . (map Tuple.swap) . Graph.labNodes

lGraphLevelSort :: LevelGraph -> [[Graph.LNode Level]]
lGraphLevelSort graph = [ lGraphGetLevel l graph | l <- levels ]
  where
    topSort = lGraphTopSort graph
    levels = List.nub $ map snd topSort

lGraphGetLevel :: Int -> LevelGraph -> [Graph.LNode Level]
lGraphGetLevel lvl graph = subList lvl 
  where
    topSort = lGraphTopSort graph
    subList l = [ (node,l) | (node,l') <- topSort, l'==l]

cGraphTopSort :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel]
cGraphTopSort = (map Tuple.swap) . List.sort . (map Tuple.swap) . Graph.labNodes

cGraphLevelSort :: CodeGraph -> [[Graph.LNode CodeGraphNodeLabel]]
cGraphLevelSort graph = [ cGraphGetLevel l graph | l <- levels ]
  where
    topSort = cGraphTopSort graph
    levels = List.sort . List.nub $ map getLevelCGN topSort
                
cGraphGetLevel :: Int -> CodeGraph -> [Graph.LNode CodeGraphNodeLabel]
cGraphGetLevel lvl graph = subList lvl
  where
    topSort = cGraphTopSort graph
    subList l = [ (node,CodeGraphNodeLabel l' ctype time) |  (node,CodeGraphNodeLabel l' ctype time) <- topSort, l'==l]


levelsLGraph :: LevelGraph -> Int
levelsLGraph  = length . List.nub . (map snd) . Graph.labNodes

levelsCGraph :: CodeGraph -> Int
levelsCGraph  = length . List.nub . (map getLevelCGN) . Graph.labNodes

subGraphFrom :: Gr a b -> Graph.Node -> Gr a b
subGraphFrom graph start = Graph.subgraph sucnodes graph
    where
      sucfn = Graph.suc graph
      getSucs [] = []
      getSucs nodes =  nodes ++ getSucs (concatMap sucfn nodes)
      sucnodes = getSucs [start]

cgGetSubFunctions :: CodeGraph -> [Graph.LNode CodeGraphNodeLabel]
cgGetSubFunctions graph = Graph.labNodes $ Graph.labnfilter isFunctionNode graph
    where
      isFunctionNode :: Graph.LNode CodeGraphNodeLabel -> Bool
      isFunctionNode (_,CodeGraphNodeLabel _ Function _) = True
      isFunctionNode (_,CodeGraphNodeLabel _ Map _) = True
      isFunctionNode _ = False

graphGetLeaves ::  Gr a b -> [Graph.LNode a]
graphGetLeaves gr = Graph.labNodes $ Graph.nfilter (null . Graph.suc gr) gr

cgSplitAtLvl :: Int -> CodeGraph ->  ([Graph.LNode CodeGraphNodeLabel], CodeGraph)
cgSplitAtLvl lvl graph =
    let
        lastlvl = head $ reverse $ cGraphLevelSort graph
        lastlvlnodes = map fst lastlvl
        restgraph = Graph.delNodes lastlvlnodes graph
    in (lastlvl, restgraph)

cgMakeLvlNamed:: Int -> [String] -> CodeGraph -> CodeGraph
cgMakeLvlNamed lvl names graph =
    let
        lvllist = List.sort . List.nub . (map getLevelCGN) . Graph.labNodes $ graph
        lvllabelednodes =  (cGraphLevelSort graph !! ((fromMaybe (-1)) $ List.elemIndex lvl lvllist)) -- should fail if Nothing
        lvlnodes = map fst lvllabelednodes
        convertFunction = (\(name, index) ->
                           (\(p,v,l,s) ->
                           if v == index
                           then (p,v,makeCGNodeNamedFunction name l,s)
                           else (p,v,l,s)
                           )
                          )
        convertFunction' graph toChange = Graph.gmap (convertFunction toChange) graph
        replaceList = zip names lvlnodes
    in foldl convertFunction' graph replaceList -- TODO: improve: this traveres graphs every time for each replace
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
condWithProb p graph oldctx@(pre,node, oldLabel,children) =
    if (length children > 3 || length children < 3 || (elem [] $ map (Graph.suc graph) (Graph.suc graph node) )) then return oldctx -- until we figure out a better way
    else newctx >>= (\x -> return [(x,p'), (oldctx,1-p') ]) >>= Control.Monad.Random.fromList
         where p' = toRational p
               randomBranches :: MonadRandom m => m [Maybe Graph.Node]
               randomBranches = knuthShuffle $ take 3 $ (map Just $ map snd children) ++ [Nothing,Nothing,Nothing]
               randomConditional = liftM mkConditional $ randomBranches
               newctx = liftM  (\x -> (pre,node, oldLabel { computationType = x, timeout = Nothing },children)) randomConditional


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
addCodeContext ctype time (pre,node,lvl,after) = (pre,node,CodeGraphNodeLabel lvl ctype time,after)

graph2LevelGraph ::  Level -> Gr () () -> LevelGraph
graph2LevelGraph level gr = Graph.buildGr (List.map (addLevelContext level) unfolded)
    where unfolded = Graph.ufold (:) [] gr

qlEdge2Edge :: Graph.LEdge () -> Graph.Edge
qlEdge2Edge (a,b,()) = (a,b)

makeCodeGraph :: ComputationType -> LevelGraph -> CodeGraph
makeCodeGraph ctype = Graph.nmap (\l -> CodeGraphNodeLabel l ctype Nothing)

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
      rootnodelabel = CodeGraphNodeLabel startlvl OtherComputation Nothing
      bigsource = (succ unocupied, CodeGraphNodeLabel (succ startlvl) SlowDataSource Nothing)
      oldEdges = Graph.labEdges graph
      oldNodes = Graph.labNodes graph
      oldNodes' = map fst $ Graph.labNodes graph
      newEdges' = [ (unocupied,node,()) | node <- oldNodes', (null $ Graph.pre graph node) ]
      newEdges = (unocupied,unocupied+1,()):newEdges'


makeCGNodeTimed :: Int -> CodeGraphNodeLabel -> CodeGraphNodeLabel
makeCGNodeTimed n node = node {timeout = Just n}

makeCGNodeNamedFunction :: String -> CodeGraphNodeLabel -> CodeGraphNodeLabel
makeCGNodeNamedFunction name node = node { computationType = NamedFunction name }

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

nodeToUniqueName :: Graph.Node -> String
nodeToUniqueName  =  (++) "local" . show

------------------------------------------------------------
-- Benchmark Code
------------------------------------------------------------
concatenateTests ::  (String -> NestedCodeGraph -> String) -> [ NestedCodeGraph ] -> [(String,String)]
concatenateTests toCodeWrapped randomGraphs = resultStrings
    where
      randomGraphsNumbered = zip [0..] randomGraphs
      totallevels = maximum $ map (levelsCGraph . fst) randomGraphs
      genName = (\(x,y) -> ("run_test_level" ++ (show $ levelsCGraph $ fst y) ++ "_" ++ show (x `quot` totallevels)))
      genCode = (\(x,y) -> toCodeWrapped (genName (x, y) ) y)
      strings = map genCode randomGraphsNumbered
      names = map genName randomGraphsNumbered
      resultStrings = zip names strings


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
