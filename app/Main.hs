{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DoAndIfThenElse #-}

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

--import Debug.Trace (trace)
import           LevelGraphs (CodeGraph, toHaskellCodeWrapped, toOhuaCodeWrapped,
                              toGraphCodeWrapped, makeCondCGWithProb, 
                              concatenateTests, genRandomCodeGraph,
                              genRandomCodeGraphBigDS, setSeed)
import           Control.Monad.Random (evalRandIO)
import           System.Console.CmdArgs
import           Control.Monad.Random (MonadRandom,fromList)
import           Control.Monad (liftM)
import qualified Data.Map.Strict                                             as Map
------------------------------------------------------------
-- Benchmark Code
------------------------------------------------------------

exampleMapUpTo :: Int -> Map.Map (Int,Int) Double
exampleMapUpTo n = Map.fromList [ ((a,b), (1 / 2^(b-a))) | a<- [1..n], b<-[1..n], a<b]

randomExampleBenchmark :: MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> Double -> Int -> m CodeGraph
--randomExampleBenchmark weightMap typeWeights ifPercentage len | trace ("randomExampleBenchmark, typeweigths: " ++ show typeWeights ++ ", ifpercentage: " ++ show ifPercentage ++ ", len: " ++ show len ++ "\n") False = undefined
randomExampleBenchmark weightMap typeWeights ifPercentage len = let lvllist = (sequence $ replicate len (Control.Monad.Random.fromList [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ])) 
                                                                    --lvllist' = liftM2 trace ((\x -> do thelist <- lvllist; return ("list: " ++ (show thelist) ++ "\n")) mylist) mylist
                                                                    in lvllist >>= genRandomCodeGraph weightMap typeWeights >>= makeCondCGWithProb ifPercentage

randomExampleBenchmarkBDS :: MonadRandom m => Map.Map (Int,Int) Double -> [Double] -> Double -> Int -> m CodeGraph
randomExampleBenchmarkBDS weightMap typeWeights ifPercentage len = (sequence $ replicate len (Control.Monad.Random.fromList [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ])) >>= genRandomCodeGraphBigDS weightMap typeWeights >>= makeCondCGWithProb ifPercentage


genExampleBenchmark :: MonadRandom m => LGCmdArgs -> m String
genExampleBenchmark lgArgs = let

    -- Options (arguments)
    lvls = levels lgArgs - 1 
    total = totalGraphs lgArgs
    lang = language lgArgs
    srcPercentage = percentageSources lgArgs
    sinkPercentage = percentageSinks lgArgs
    ifPercentage = percentageIfs lgArgs
    slowDS = slowdatasource lgArgs

    -- Derivated data structures
    lvllist = take total $ foldl (\x _ -> x ++ [lvls,(lvls-1)..0]) [] [1..total]
    weightMap = exampleMapUpTo lvls
    typeWeights = [srcPercentage,sinkPercentage]
    toCodeWrapped = case lang of
                      "Haskell" -> toHaskellCodeWrapped
                      "Ohua" ->  toOhuaCodeWrapped
                      "Graph" -> toGraphCodeWrapped
                      _ -> (\_ _ -> "Unexpected language case error")
    randomBenchmark = if slowDS then randomExampleBenchmarkBDS else randomExampleBenchmark 
    in liftM (concatenateTests toCodeWrapped) $ sequence (map (randomBenchmark weightMap typeWeights ifPercentage) lvllist)


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
                            percentageSources :: Double,
                            percentageSinks :: Double,
                            percentageIfs :: Double,
                            slowdatasource :: Bool,
                            preamble :: Maybe FilePath
                           } deriving (Show, Data, Typeable)

lgCmdArgs :: LGCmdArgs
lgCmdArgs = LGCmdArgs {output = "" &= name "o" &= help "Output to file. If no output file nor a namespace is given, only the graph code is output to stdout.",
                       levels = 10 &= name "l" &= help "When several graphs are generated, this gives the maximum number of levels the graphs will have. Will generate graphs having l levels, l-1 levels, and so forth until 1. They will be repeated once the list is finished, if there are more graphs than levels. Default is 10",
                       totalGraphs = 1 &= name "n" &= help "Total number of graphs to generate. Default is 1",
                       language = "Ohua" &= name "L" &= help "Language to outpt in. \"Graph\" for graphs. Default is Ohua.",
                       seed = (-1) &= name "s" &= help "Random seed for ensuring reproducibility (positive integer). Default is random.",
                       percentageSources = 0.4 &= help "Percentage of nodes that shall be data sources. It must add up to 1 with the percentages for sinks, and executes (implicit). Default is 0.4",
                       percentageSinks = 0 &= help "Percentage of nodes that shall be data sources. It must add up to 1 with the percentages for sources, and executes (implicit). Default is 0",
                       percentageIfs = 0 &= help "Percentage of nodes that shall be conditionals (ifs). Must be between 0 and 1. Independent of sources, sinks and executes (is applied *in the end*). Default is 0",
                       preamble = def &= name "p" &= help "Prepend some code to the generated code.",
                       slowdatasource = False &= name "S" &= help "Include a slow data source at one side."
                      }
            &= summary "Level-graphs: generates random level graphs, v-0.1.0.0"

checkArgs :: LGCmdArgs -> IO Bool
checkArgs lgArgs = do
  errorOcurred <- return False
  let l = levels lgArgs
  errorOcurred <- if l <= 0 then
               do
                 print "Error: Non-positive level!"
                 return True
           else
               do
                 return errorOcurred

  let n = totalGraphs lgArgs
  errorOcurred <- if n < 0 then
               do
                 print "Error: Negative number of graphs!"
                 return True
           else
               do
                 return errorOcurred
  let lang = language lgArgs
  errorOcurred <- if (lang == "Ohua" || lang == "Haskell" || lang == "Graph") then
               do
                 return errorOcurred
           else
               do
                 print "Error: Unrecognized language! (maybe not capitalized?)"
                 return True
  let s = seed lgArgs
  errorOcurred <- if (s < 0 && s /= (-1)) then
               do
                 print "Error: Negative seed!"
                 return True
           else
               do
                 return errorOcurred
  let ifPercentage = percentageIfs lgArgs
      srcPercentage = percentageSources lgArgs
      sinkPercentage = percentageSinks lgArgs
      totalPrecentages = sinkPercentage + srcPercentage
      conditionPercentages = (srcPercentage < 0) || (srcPercentage > 1) ||
                             (sinkPercentage < 0) || (sinkPercentage > 1) ||
                             (ifPercentage < 0) || (ifPercentage > 1) ||
                             (totalPrecentages > 1)
  errorOcurred <- if (conditionPercentages) then
               do
                 print "Error: Percentages for node types must be between 0 and 1. Percentages for source and sink must add to <= 1 (the rest is implicitly the percentage for compute nodes)"
                 return True
           else
               do
                 return errorOcurred

  return errorOcurred

-- ----------------
--      main
-- ----------------
main :: IO ()
main = do
  lgArgs <- cmdArgs lgCmdArgs
  errorOcurred <- checkArgs lgArgs

  if errorOcurred == True then
      return ()
  else
      do
        -- Main execution branch

        -- Setup (seed, output file)
        setSeed (seed lgArgs)
        let outputFile = output lgArgs

        -- Execute benchmark
        outputString <- Control.Monad.Random.evalRandIO (genExampleBenchmark lgArgs)

        outputString <- case preamble lgArgs of
                            Nothing -> return outputString
                            Just file -> liftM (++ outputString) $ readFile file

        -- Print it accordingly
        if outputFile == "" then
            putStrLn outputString
        else
            writeFile outputFile outputString

  return ()
