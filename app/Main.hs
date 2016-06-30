{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables, ExplicitForAll #-}

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
import           LevelGraphs (CodeGraph, CodeSubGraphs, NestedCodeGraph,
                              makeCodeGraphRandomlyTimed,
                              makeNestedCodeGraphRandomlyTimed,
                              nodeToUniqueName, cgGetSubFunctions,
                              makeCondCGWithProb, concatenateTests, listTests,
                              genRandomCodeGraph, genRandomCodeGraphBigDS)
import           Backend (toCodeWrapped)
import           Control.Monad.Random (runRand, evalRand)
import           Control.Monad
import qualified System.Random (mkStdGen, getStdGen, StdGen, random)
import           Data.Traversable (mapAccumL)
import           Data.Tuple (swap)
import           Data.List (intercalate)
import           Data.Tuple.Sequence
import           System.Console.CmdArgs
import           Control.Arrow (first)
import           Data.Maybe (fromMaybe,fromJust,isJust, catMaybes)
import           Control.Monad.Random (MonadRandom,fromList)
import           Control.Monad (liftM)
import qualified Data.Graph.Inductive as Graph
import qualified Data.Map.Strict                                             as Map
import Debug.Trace (traceShowId)
------------------------------------------------------------
-- Benchmark Code
------------------------------------------------------------

exampleMapUpTo :: Int -> Map.Map (Int,Int) Double
exampleMapUpTo n = Map.fromList [ ((a,b), (1 / 2^(b-a))) | a<- [1..n], b<-[1..n], a<b]

zipMon :: Monad m => m [a] -> [b] -> m [(a,b)] -- there is probably a std lib function for this
zipMon as bs = liftM2 zip as $ return bs

generateSubGraphs :: forall m. MonadRandom m => ([Int] -> m CodeGraph) -> Int -> CodeGraph -> m CodeSubGraphs
generateSubGraphs generatingFunction remainingDepth graph = liftM2 (++) subgraphsFull rest
-- I probably will run into a name clash here. Either give them different unique names or find a way to nest the contexts in the backend
  where
    subnodes = cgGetSubFunctions graph
    makeNode = fst
    names = map (\s -> "ifn" ++ (nodeToUniqueName $ makeNode s)) subnodes :: [String]
    mkSubgraph lnode = generatingFunction [1,length $ Graph.suc graph $ makeNode lnode]
    mkSubgraphNoDepth lnode = generatingFunction []  -- this should yield no further iterations
    currentSubgraphs :: m [CodeGraph]
    currentSubgraphs = mapM (if remainingDepth <= 0 then mkSubgraphNoDepth else mkSubgraph) subnodes
    subgraphsFull = zipMon currentSubgraphs names
    continueGenerating :: CodeGraph -> m CodeSubGraphs
    continueGenerating = generateSubGraphs generatingFunction (traceShowId $ remainingDepth - 1)
    rest
        | remainingDepth <= 0 = return [] :: m CodeSubGraphs
        | otherwise = do
            gs <- currentSubgraphs -- :: [CodeGraph]
            listOfSubGraphs <-  mapM continueGenerating gs :: m [CodeSubGraphs]
            let oneSubGraphs = concat listOfSubGraphs
            return oneSubGraphs :: m CodeSubGraphs

randomExampleBenchmark :: forall m . MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> Double -> Int -> m NestedCodeGraph
--randomExampleBenchmark weightMap typeWeights ifPercentage len | trace ("randomExampleBenchmark, typeweigths: " ++ show typeWeights ++ ", ifpercentage: " ++ show ifPercentage ++ ", len: " ++ show len ++ "\n") False = undefined
randomExampleBenchmark weightMap typeWeights ifPercentage len = sequenceT (mainGraph, subGraphs)
  where
    lvllist :: m [Int]
    lvllist = (sequence $ replicate len (Control.Monad.Random.fromList [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ]))
    --lvllist' = liftM2 trace ((\x -> do thelist <- lvllist; return ("list: " ++ (show thelist) ++ "\n")) mylist) mylist
    generatingFunction :: [Int] -> m CodeGraph
    generatingFunction = genRandomCodeGraph weightMap typeWeights >=> makeCondCGWithProb ifPercentage
    mainGraph = lvllist >>= generatingFunction
    subGraphs = mainGraph >>= (generateSubGraphs generatingFunction 1) -- TODO: make depth not hard-coded!

randomExampleBenchmarkBDS :: forall m . MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> Double -> Int -> m NestedCodeGraph
randomExampleBenchmarkBDS weightMap typeWeights ifPercentage len =
    let
        lvllist :: m [Int]
        lvllist = (sequence $ replicate len (Control.Monad.Random.fromList [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ]))
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
    typeWeights = [srcPercentage,sinkPercentage, funPercentage, mapPercentage]
    concatenateFun = concatenateTests
    randomBenchmark' :: MonadRandom m => Int -> m NestedCodeGraph
    randomBenchmark' lvl
        | slowDS = randomExampleBenchmarkBDS weightMap typeWeights ifPercentage lvl
        | otherwise = randomExampleBenchmark weightMap typeWeights ifPercentage lvl

    randomBenchmark :: System.Random.StdGen -> Int -> (NestedCodeGraph, System.Random.StdGen)
    randomBenchmark gen lvl
        | isJust cache =
            Control.Arrow.first (\x -> flip evalRand gen
                $ makeNestedCodeGraphRandomlyTimed (fromJust cache) x)
                -- first f '=' (f,id)
                $ runRand (randomBenchmark' lvl) gen
        | otherwise = runRand (randomBenchmark' lvl) gen
        -- case lang of
        --                "HaskellDo" -> (\x y -> concatenateTests x y ++ "\nallTests :: [((Env u -> IO Int),Int,Int)]\nallTests = " ++ listTests y)
        --                "HaskellDoApp" -> (\x y -> concatenateTests x y ++ "\nallTests :: [((Env u -> IO Int),Int,Int)]\nallTests = " ++ listTests y) -- The same as HaskellDo
        --                _ -> concatenateTests


--  graphs <- Control.Monad.Random.evalRandIO singleString
--  putStrLn graphs


------------------------------------------------------------
-- Command-Line Arguments Parsing
------------------------------------------------------------

data LGCmdArgs = LGCmdArgs {output :: String,
                            levels :: Int,
                            totalGraphs :: Int,
                            language :: String,
                            seed :: Int,
                            maxDepth :: Int,
                            percentageSources :: Double,
                            percentageSinks :: Double,
                            percentageFuns :: Double,
                            percentageMaps :: Double,
                            percentageIfs :: Double,
                            slowdatasource :: Bool,
                            cachenum :: Maybe Int,
                            preamble :: Maybe FilePath
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
    , preamble = def &= name "p" &= help "Prepend some code to the generated code."
    , cachenum = Nothing &= name "c" &= help "Make a results that are cachable. Will generate from c possible requests. If flag is not present, caching is off. In this case all requests are different."
    , slowdatasource = False &= name "S" &= help "Include a slow data source at one side."
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
  })
  = catMaybes
      [ testThat (l <= 0) "Error: Non-positive level!"
      , testThat (fromMaybe 1 c <= 0) "Error: Non-positive nomber request types (cache)!"
      , testThat (n < 0) "Error: Negative number of graphs!"
      , testThat (lang `notElem` map fst acceptedLanguages) "Error: Unrecognized language! (maybe not capitalized?)"
      , testThat (s < -1) "Error: Negative seed!"
      , testThat (d < 0) "Error: Negative (max) depth!"
      , testThat
            ((srcPercentage < 0) || (srcPercentage > 1) ||
             (sinkPercentage < 0) || (sinkPercentage > 1) ||
             (ifPercentage < 0) || (ifPercentage > 1) ||
             (totalPrecentages > 1))
            "Error: Percentages for node types must be between 0 and 1. Percentages for source and sink must add to <= 1 (the rest is implicitly the percentage for compute nodes)"
      ]
  where totalPrecentages = sinkPercentage + srcPercentage + funPercentage + mapPercentage
-- --------------------
--    output functions
-- --------------------

acceptedLanguages :: [(String, String)]
acceptedLanguages =
    [ ("Ohua", ".clj")
    , ("OhuaApp", ".clj")
    , ("HaskellDoApp", ".hs")
    , ("HaskellDo", ".hs")
    , ("MuseApp", ".clj")
    , ("MuseMonad", ".clj")
    , ("Graph", "")
    ]

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
            writeToFiles outputFile (fromJust $ lookup (language lgArgs) acceptedLanguages) outputStrings
