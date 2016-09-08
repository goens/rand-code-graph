{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- | Code for generating random level-graphs, a generalization of trees
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

--import Debug.Trace (trace)
import           Backend                    (acceptedLanguages,
                                             inlineIfBranches, toCodeWrapped)
import           Control.Arrow              (first, second)
import           Control.Monad
import           Control.Monad              (liftM)
import           Control.Monad.Random       (evalRand, runRand)
import           Control.Monad.Random       (MonadRandom, fromList)
import           Control.Monad.Random.Class (getRandom)
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (evalStateT)
import           Data.Graph.Inductive       as Graph
import           Data.IORef
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             isJust)
import           Data.Traversable           (mapAccumL)
import           Data.Tuple                 (swap)
import           Data.Tuple.Sequence
import           LevelGraphs                (Arity, CodeGraph,
                                             CodeGraphNodeLabel (..),
                                             CodeSubGraphs,
                                             ComputationType (..),
                                             FunctionMeta (..), NestedCodeGraph,
                                             cgGetSubFunctions,
                                             concatenateTests,
                                             genRandomCodeGraph,
                                             genRandomCodeGraphBigDS,
                                             isFunctionNode, listTests,
                                             makeCodeGraphRandomlyTimed,
                                             makeCondCGWithProb,
                                             makeNestedCodeGraphRandomlyTimed,
                                             nodeToUniqueName, traceWith)
import           System.Console.CmdArgs
import qualified System.Random              (StdGen, getStdGen, mkStdGen,
                                             random)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
------------------------------------------------------------
-- Benchmark Code
------------------------------------------------------------

--

maxLevelWidth :: IORef Int
maxLevelWidth = unsafePerformIO $ newIORef 6

relabelNodes :: MonadState Int m => [CodeGraph] -> m [CodeGraph]
relabelNodes subgraphs = mapM f subgraphs
    where
        f graph = do
            s <- get
            let f2 (_, index, label, _) = insNode (index + s, label)
                g = ufold f2 (Graph.empty :: Gr CodeGraphNodeLabel ()) graph
                g2 = foldl (\gr (n1, n2) -> insEdge (n1 + s, n2+s, ()) gr) g (edges graph)
            let (_, upper) = Graph.nodeRange g2
            put $ succ upper
            return g2


exampleMapUpTo :: Int -> Map.Map (Int,Int) Double
exampleMapUpTo n = Map.fromList [ ((a,b), (1 / 2^(b-a))) | a<- [1..n], b<-[1..n], a<b]

zipMon :: Monad m => m [a] -> [b] -> m [(a,b)] -- there is probably a std lib function for this
zipMon as bs = liftM2 zip as $ return bs       -- There isn't but there's this, much cleaner, implementation

removeFuncs :: CodeGraph -> CodeGraph
removeFuncs = Graph.nmap inner
  where
    inner label = label
      { computationType =
          case computationType label of
            Map -> DataSource
            NamedFunction _ -> OtherComputation
            Function -> OtherComputation
            a -> a
      }


generateSubGraphs :: (MonadRandom m, MonadState Int m) => ([Int] -> m CodeGraph) -> Int -> CodeGraph -> m CodeSubGraphs
generateSubGraphs generatingFunction remainingDepth graph
    | remainingDepth <= 0 = do
      gs <- currentSubgraphs
      let sanitized = map removeFuncs gs
      return $ zipWith (\gen gr -> gen {fn=gr}) functionGens sanitized
    | otherwise = do
        gs <- currentSubgraphs
        listOfSubGraphs <- mapM continueGenerating gs
        let oneSubGraphs = concat listOfSubGraphs
        return $ zipWith (\gen gr -> gen {fn=gr}) functionGens gs ++ oneSubGraphs
-- I probably will run into a name clash here. Either give them different unique names or find a way to nest the contexts in the backend
-- DONE: By renaming all subgraph nodes, see the hack below
  where
    subnodes = cgGetSubFunctions graph
    functionGens = concatMap (\lnode@(node, label) ->
        let uname = nodeToUniqueName node
            gen = generatingFunction [1,length $ Graph.suc graph node]
            arity = calculateArity graph lnode
            mfn = FunctionMeta{fn=gen, fnArity=arity, fnIsMapped=isMapOp label}
        in if isConditional label
              then [ mfn {fnName="ifnthen" ++ uname}
                   , mfn {fnName="ifnelse" ++ uname}
                   ]
              else [ mfn {fnName="ifn" ++ uname} ]
            ) subnodes
    currentSubgraphs =
      sequence (map fn functionGens)
      >>= relabelNodes -- HACK: A dirt hack to handle name clashes
                       -- It'd be much better if there was a way to tell the backend which ids not to use
                       -- and/or where to start counting from
    continueGenerating = generateSubGraphs generatingFunction (remainingDepth - 1)


isConditional :: CodeGraphNodeLabel -> Bool
isConditional = go . computationType
  where go Conditional{} = True
        go _ = False


isMapOp :: CodeGraphNodeLabel -> Bool
isMapOp = go . computationType
  where go Map = True
        go _ = False


calculateArity :: CodeGraph -> LNode CodeGraphNodeLabel -> Arity
-- calculateArity _ _ = 1 -- HACK (temporary)
calculateArity _ (_, CodeGraphNodeLabel _ Map _ _) = 1
calculateArity graph (a, _) = length $ suc graph a


gufoldM :: (Graph gr, Monad m) => (Context a b -> c -> m c) -> c -> gr a b -> m c
gufoldM f u g
  | isEmpty g = return u
  | otherwise = gufoldM f u g' >>= f c
  where
    (c,g') = matchAny g


ggmapM :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
ggmapM f = gufoldM (\c gr -> (Graph.& gr) <$> f c) empty


levelWidthPercentages :: [(Int, Rational)]
levelWidthPercentages = take newBase $ map (second (fac *)) [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ]
    where newBase = unsafePerformIO $ readIORef maxLevelWidth
          fac = realToFrac newBase / 6


randomExampleBenchmark :: MonadRandom m => Bool -> Map.Map (Int,Int) Double -> [Double] -> Double -> Int -> m NestedCodeGraph
--randomExampleBenchmark weightMap typeWeights ifPercentage len | trace ("randomExampleBenchmark, typeweigths: " ++ show typeWeights ++ ", ifpercentage: " ++ show ifPercentage ++ ", len: " ++ show len ++ "\n") False = undefined
randomExampleBenchmark withBigDS weightMap typeWeights ifPercentage len = do
    gr <- mainGraph
    let (_, upper) = Graph.nodeRange gr
    subgr <- flip evalStateT upper $ generateSubGraphs generatingFunction 1 gr  -- TODO: make depth not hard-coded!
    subgr <- flip mapM subgr $ \subgraph -> do
      let graph = fn subgraph
      let (_, upper) = Graph.nodeRange graph
      parents <- case filter (not . isFunctionNode . snd) $ Graph.labNodes graph of
                    [] -> return []
                    a -> do
                      num <- getRandom
                      return $ take (num `mod` 5 :: Int) $ cycle a
      let ids = [succ upper..]
      let newGr = foldr (\(id, (pid, label)) -> Graph.insEdge (pid, id, ()) . insNode (id, CodeGraphNodeLabel (succ $ LevelGraphs.level label) DataSource Nothing Nothing) ) graph (zip ids parents)
      return $ subgraph {fn =newGr}


    return (gr, subgr)
  where
    lvllist = (sequence $ replicate len (Control.Monad.Random.fromList levelWidthPercentages))
    --lvllist' = liftM2 trace ((\x -> do thelist <- lvllist; return ("list: " ++ (show thelist) ++ "\n")) mylist) mylist

    -- This type signature is necessary to make the return independant of the
    -- concrete monad, since this is bound once in MonadRandom m and the StateT
    -- wrapped monad.
    genGraph :: MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> [Int] -> m CodeGraph
    genGraph
        | withBigDS = genRandomCodeGraphBigDS
        | otherwise = genRandomCodeGraph
    generatingFunction :: MonadRandom m => [Int] -> m CodeGraph
    generatingFunction =
      genGraph weightMap typeWeights
      >=> makeCondCGWithProb ifPercentage
      >=> removeEmptyMaps -- HACK
                          -- Eventually the backend should either not generate maps on functions with no sucessors
                          -- OR ohua fixes its handling of empty vectors :P
    mainGraph = lvllist >>= generatingFunction


removeEmptyMaps :: MonadRandom m => CodeGraph -> m CodeGraph
removeEmptyMaps graph = ggmapM (\(a, nodeId, label, b) -> (a,nodeId,,b) <$> f nodeId label) graph
  where
    f nodeId label =
      case computationType label of
          Map | length (suc graph nodeId) < 2 -> do
              num <- getRandom
              return $ label { computationType = [OtherComputation, DataSource] !! (num `mod` 2 :: Int) }
          -- HACK (temporary)
          Function | length (suc graph nodeId) < 2 -> do
              num <- getRandom
              return $ label { computationType = [OtherComputation, DataSource] !! (num `mod` 2 :: Int) }
          -- HACK (temporary)
          NamedFunction _ | length (suc graph nodeId) < 2 -> do
              num <- getRandom
              return $ label { computationType = [OtherComputation, DataSource] !! (num `mod` 2 :: Int) }
          _ -> return label


randomExampleBenchmarkBDS :: forall m . MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> Double -> Int -> m NestedCodeGraph
randomExampleBenchmarkBDS weightMap typeWeights ifPercentage len =
    let
        lvllist :: m [Int]
        lvllist = (sequence $ replicate len (Control.Monad.Random.fromList levelWidthPercentages))
        mainGraph = lvllist >>= genRandomCodeGraphBigDS weightMap typeWeights >>= makeCondCGWithProb ifPercentage
        subGraphs = return [] :: m CodeSubGraphs -- TODO: generate subgraphs (recursively!)
    in sequenceT (mainGraph, subGraphs)


genExampleBenchmark :: LGCmdArgs -> [(String, String)] -- Name, Code
genExampleBenchmark
    lgArgs@(LGCmdArgs
        { totalGraphs = total
        , language = lang
        , percentageSources = srcPercentage
        , percentageSinks = sinkPercentage
        , percentageFuns = funPercentage
        , percentageMaps = mapPercentage
        , percentageIfs = ifPercentage
        , percentageSlow = slowPercentage
        , slowdatasource = slowDS
        , cachenum = cache
        })
    = concatenateFun (toCodeWrapped lang) $ snd $ mapAccumL (\x y -> swap $ randomBenchmark x y) stdGen lvllist
  where
    lvls = levels lgArgs - 1
    stdGen = System.Random.mkStdGen $ seed lgArgs

    -- Derivated data structures
    lvllist = take total $ cycle [lvls,(lvls-1)..0]
    weightMap = exampleMapUpTo lvls
    typeWeights = [srcPercentage,sinkPercentage, funPercentage, mapPercentage, slowPercentage]
    concatenateFun = concatenateTests
    randomBenchmark' :: MonadRandom m => Int -> m NestedCodeGraph
    randomBenchmark' lvl = randomExampleBenchmark slowDS weightMap typeWeights ifPercentage lvl

    randomBenchmark :: System.Random.StdGen -> Int -> (NestedCodeGraph, System.Random.StdGen)
    randomBenchmark gen lvl
        | isJust cache = flip runRand gen $ do
            randomBenchmark' lvl >>= makeNestedCodeGraphRandomlyTimed (fromJust cache)
        | otherwise = runRand (evalStateT (randomBenchmark' lvl) 0) gen
        -- case lang of
        --                "HaskellDo" -> (\x y -> concatenateTests x y ++ "\nallTests :: [((Env u -> IO Int),Int,Int)]\nallTests = " ++ listTests y)
        --                "HaskellDoApp" -> (\x y -> concatenateTests x y ++ "\nallTests :: [((Env u -> IO Int),Int,Int)]\nallTests = " ++ listTests y) -- The same as HaskellDo
        --                _ -> concatenateTests


--  graphs <- Control.Monad.Random.evalRandIO singleString
--  putStrLn graphs


------------------------------------------------------------
-- Command-Line Arguments Parsing
------------------------------------------------------------

data LGCmdArgs = LGCmdArgs {output            :: String,
                            levels            :: Int,
                            totalGraphs       :: Int,
                            language          :: String,
                            seed              :: Int,
                            maxDepth          :: Int,
                            percentageSources :: Double,
                            percentageSinks   :: Double,
                            percentageFuns    :: Double,
                            percentageMaps    :: Double,
                            percentageIfs     :: Double,
                            percentageSlow    :: Double,
                            levelWidth        :: Int,
                            slowdatasource    :: Bool,
                            cachenum          :: Maybe Int,
                            preamble          :: Maybe FilePath,
                            inlineIf          :: Bool
                           } deriving (Show, Data, Typeable)

lgCmdArgs :: LGCmdArgs
lgCmdArgs = LGCmdArgs
    { output = "" &= name "o" &= help "Output to file (prefix). If no output file nor a namespace is given, only the graph code is output to stdout."
    , levels = 10 &= name "l" &= help "When several graphs are generated, this gives the maximum number of levels the graphs will have. Will generate graphs having l levels, l-1 levels, and so forth until 1. They will be repeated once the list is finished, if there are more graphs than levels. Default is 10"
    , totalGraphs = 1 &= name "n" &= help "Total number of graphs to generate. Default is 1"
    , language = "Ohua" &= name "L" &= help "Language to outpt in. \"Graph\" for graphs. Default is Ohua."
    , seed = (-1) &= name "s" &= help "Random seed for ensuring reproducibility (positive integer). Default is random."
    , maxDepth = (2) &= help "Maximum nesting depth for functions in generated graphs. Default is 2."
    , percentageSources = 0.4 &= help "Percentage of nodes that shall be data sources. It must add up to 1 with the percentages for sinks, and executes (implicit). Default is 0.4"
    , percentageSinks = 0 &= help "Percentage of nodes that shall be data sources. It must add up to 1 with the percentages for sources, and executes (implicit). Default is 0"
    , percentageFuns = 0 &= help "Percentage of nodes that shall be functions (with their own code graph). It must add up to 1 with the percentages for sources, and executes (implicit). Default is 0"
    , percentageMaps = 0 &= help "Percentage of nodes that shall be invocations of the higher-order function 'map' (or derivatives thereof). It must add up to 1 with the percentages for sources, and executes (implicit). Default is 0"
    , percentageIfs = 0 &= help "Percentage of nodes that shall be conditionals (ifs). Must be between 0 and 1. Independent of sources, sinks and executes (is applied *in the end*). Default is 0"
    , percentageSlow = 0 &= help "Percentage of slow data sources to use. Default 0"
    , levelWidth = 6 &= help "Maximum width of levels"
    , preamble = def &= name "p" &= help "Prepend some code to the generated code."
    , cachenum = Nothing &= name "c" &= help "Make a results that are cachable. Will generate from c possible requests. If flag is not present, caching is off. In this case all requests are different."
    , slowdatasource = False &= name "S" &= help "Include a slow data source at one side."
    , inlineIf = False &= name "i"
    } &= summary "Level-graphs: generates random level graphs, v-0.1.0.0"


testThat :: Bool -> String -> Maybe String
testThat True s = Just s
testThat _ _ = Nothing


checkArgs :: LGCmdArgs -> [String]
checkArgs (LGCmdArgs
  { levels = l
  , cachenum = c
  , totalGraphs = n
  , language = lang
  , seed = s
  , maxDepth = d
  , percentageIfs = ifPercentage
  , percentageSources = srcPercentage
  , percentageSinks = sinkPercentage
  , percentageFuns = funPercentage
  , percentageMaps = mapPercentage
  , percentageSlow = slowPercentage
  })
  = catMaybes
      [ testThat (l <= 0) "Error: Non-positive level!"
      , testThat (fromMaybe 1 c <= 0) "Error: Non-positive nomber request types (cache)!"
      , testThat (n < 0) "Error: Negative number of graphs!"
      , testThat (lang `notElem` Map.keys acceptedLanguages) "Error: Unrecognized language! (maybe not capitalized?)"
      , testThat (s < -1) "Error: Negative seed!"
      , testThat (d < 0) "Error: Negative (max) depth!"
      , testThat
            ((srcPercentage < 0) || (srcPercentage > 1) ||
             (sinkPercentage < 0) || (sinkPercentage > 1) ||
             (ifPercentage < 0) || (ifPercentage > 1) ||
             (totalPrecentages > 1))
            "Error: Percentages for node types must be between 0 and 1. Percentages for source and sink must add to <= 1 (the rest is implicitly the percentage for compute nodes)"
      ]
  where totalPrecentages = sinkPercentage + srcPercentage + funPercentage + mapPercentage + slowPercentage
-- --------------------
--    output functions
-- --------------------


prepareOutputStrings :: FilePath -> [ (String,String) ] -> IO [(String,String)]
prepareOutputStrings preambleFile graphStrings = sequence $ map appendPre graphStrings
  where
    appendPre :: (String, String) -> IO (String, String)
    appendPre (name, code) = do
        pre <- readFile preambleFile
        return (name, pre ++ code)

printSerialized :: [(String,String)] -> IO ()
printSerialized codeGraphs =
    let
        codeStrings = map snd codeGraphs
        serial = intercalate "\n" codeStrings
    in putStrLn serial

writeToFiles :: String -> String -> [(String,String)] -> IO ()
writeToFiles filename fileExt codeGraphs =
    let
        codeStrings = map snd codeGraphs
        names = map fst codeGraphs
        writeToFile name code = writeFile (filename ++ name ++ fileExt) code
        outputsToFile = zipWith writeToFile names codeStrings
    in void $ sequence outputsToFile
-- ----------------
--      main
-- ----------------
main :: IO ()
main = do
  lgArgs <- cmdArgs lgCmdArgs
  let errorsOcurred = checkArgs lgArgs

  if not $ null errorsOcurred then do
      putStrLn "Unexpected values in command line arguments:"
      mapM_ putStrLn errorsOcurred
  else
      do
        -- Main execution branch

        writeIORef inlineIfBranches (inlineIf lgArgs)
        writeIORef maxLevelWidth (levelWidth lgArgs)

        randSeed <- if (seed lgArgs) == (-1)
                        then do
                            randSeed <- fst . System.Random.random <$> System.Random.getStdGen
                            putStrLn $ "No seed provided, running with random seed: " ++ show randSeed
                            return randSeed
                        else return $ seed lgArgs
        -- Setup (seed, output file)
        --setSeed (seed lgArgs)
        let outputFile = output lgArgs

        -- Execute benchmark
        let outputStrings = genExampleBenchmark lgArgs { seed = randSeed }

        -- Print it accordingly
        outputStrings <- case preamble lgArgs of
                            Nothing -> return outputStrings
                            Just file -> prepareOutputStrings file outputStrings

        if outputFile == "" then
            printSerialized outputStrings
        else
            writeToFiles outputFile (fst $ fromJust $ Map.lookup (language lgArgs) acceptedLanguages) outputStrings
