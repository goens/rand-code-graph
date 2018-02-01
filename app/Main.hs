{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DoAndIfThenElse     #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
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
import           Backend                    (acceptedLanguages, toCodeWrapped)
import           Backend.Language.Common    (Serialized, fnName')
import           Control.Arrow              (first, second)
import           Control.Monad
import           Control.Monad              (liftM)
import           Control.Monad.Random       (evalRand, runRand)
import           Control.Monad.Random       (MonadRandom, fromList)
import           Control.Monad.Random.Class (getRandom)
import           Control.Monad.Reader
import           Control.Monad.State.Class
import           Control.Monad.State.Strict (evalStateT)
import           Data.Graph.Inductive       as Graph
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe,
                                             isJust)
import           Data.Monoid
import           Data.Ratio
import           Data.String
import           Data.Traversable           (mapAccumL)
import           Data.Tuple                 (swap)
import           Data.Tuple.Sequence
import           LevelGraphs                as LG (Arity(..), CodeGraph,
                                                   CodeGraphNodeLabel(..),
                                                   CodeSubGraphs,
                                                   ComputationType(..),
                                                   GeneratorConfig(..),
                                                   NestedCodeGraph, ReadConf,
                                                   SubFunctionSpecProvider,
                                                   cgGetSubFunctions,
                                                   concatenateTests,
                                                   genRandomCodeGraph,
                                                   makeCodeGraphRandomlyTimed,
                                                   makeCondCGWithProb,
                                                   makeNestedCodeGraphRandomlyTimed,
                                                   nodeToUniqueName, traceWith)
import           Options.Applicative
import qualified System.Random              (StdGen, getStdGen, mkStdGen,
                                             random)
------------------------------------------------------------
-- Benchmark Code
------------------------------------------------------------

--
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


exampleMapUpTo :: Int -> Map.Map (Int,Int) Rational
exampleMapUpTo n = Map.fromList [ ((a,b), (1 / 2^(b-a))) | a<- [1..n], b<-[1..n], a<b]

zipMon :: Monad m => m [a] -> [b] -> m [(a,b)] -- there is probably a std lib function for this
zipMon as bs = liftM2 zip as $ return bs       -- There isn't but there's this, much cleaner, implementation


generateSubGraphs :: (MonadRandom m, MonadState Int m, ReadConf m) => ([Int] -> m CodeGraph) -> Int -> CodeGraph -> m CodeSubGraphs
generateSubGraphs generatingFunction remainingDepth graph = do
  subnodeProviders <- cgGetSubFunctions graph
  let specs = subnodeProviders >>= (\((x,_), f) -> f x (outdeg graph x))
      subnodes = map fst subnodeProviders
      currentSubgraphs =
        mapM (\lnode -> generatingFunction [1,length $ Graph.suc graph $ fst lnode]) subnodes
        >>= relabelNodes -- HACK: A dirt hack to handle name clashes
                         -- It'd be much better if there was a way to tell the backend which ids not to use
                         -- and/or where to start counting from
      (names, arities) = unzip specs
  if remainingDepth <= 0
    then do
      isFunctionNode <- (isJust .) <$> asks functionNodes
      -- this removes anything classified as a function node entirely from the generation
      local (\s -> s { LG.percentages = filter (not . isFunctionNode . fst) $ LG.percentages s }) $ do
        gs <- currentSubgraphs
        return $ zip3 gs names arities
    else do
      gs <- currentSubgraphs
      listOfSubGraphs <- mapM continueGenerating gs
      let oneSubGraphs = concat listOfSubGraphs
      return $ zip3 gs names arities ++ oneSubGraphs
-- I probably will run into a name clash here. Either give them different unique names or find a way to nest the contexts in the backend
-- DONE: By renaming all subgraph nodes, see the hack below
  where
    continueGenerating = generateSubGraphs generatingFunction (remainingDepth - 1)


gufoldM :: (Graph gr, Monad m) => (Context a b -> c -> m c) -> c -> gr a b -> m c
gufoldM f u g
  | isEmpty g = return u
  | otherwise = gufoldM f u g' >>= f c
  where
    (c,g') = matchAny g


ggmapM :: (DynGraph gr, Monad m) => (Context a b -> m (Context c d)) -> gr a b -> m (gr c d)
ggmapM f = gufoldM (\c gr -> (Graph.& gr) <$> f c) Graph.empty


lvllist :: MonadRandom m => Int -> m [Int]
lvllist len = sequence $ replicate len (Control.Monad.Random.fromList [(1,0.1), (2,0.3), (3,0.4), (4,0.1), (5,0.07), (6,0.03) ])

randomExampleBenchmark :: (MonadRandom m, ReadConf m) => Map.Map (Int,Int) Rational -> Int -> m NestedCodeGraph
--randomExampleBenchmark weightMap typeWeights ifPercentage len | trace ("randomExampleBenchmark, typeweigths: " ++ show typeWeights ++ ", ifpercentage: " ++ show ifPercentage ++ ", len: " ++ show len ++ "\n") False = undefined
randomExampleBenchmark weightMap len = do
  isFnNode <- (isJust .) <$> asks functionNodes
  gr <- lvllist len >>= generatingFunction
  let (_, upper) = Graph.nodeRange gr
  subgr <- flip evalStateT upper $ generateSubGraphs generatingFunction 1 gr  -- TODO: make depth not hard-coded!
  subgr <- flip mapM subgr $ \(graph, name, arity) -> do
    let (_, upper) = Graph.nodeRange graph
    parents <- case filter (not . isFnNode . computationType . snd) $ Graph.labNodes graph of
                 [] -> return []
                 a -> do
                   num <- getRandom
                   return $ take (num `mod` 5 :: Int) $ cycle a
    let ids = [succ upper..]
    let newGr = foldr (\(id, (pid, CodeGraphNodeLabel l _ _)) -> Graph.insEdge (pid, id, ()) . insNode (id, CodeGraphNodeLabel (succ l) "get-data" Nothing ) ) graph (zip ids parents)
    return (newGr, name, arity)


  return (gr, subgr)
  where
    --lvllist' = liftM2 trace ((\x -> do thelist <- lvllist; return ("list: " ++ (show thelist) ++ "\n")) mylist) mylist

    -- This type signature is necessary to make the return independant of the
    -- concrete monad, since this is bound once in MonadRandom m and the StateT
    -- wrapped monad.
    generatingFunction :: (ReadConf m, MonadRandom m) => [Int] -> m CodeGraph
    generatingFunction = genRandomCodeGraph weightMap >=> makeCondCGWithProb


-- TODO fill this with information about function types
stdFunctionTypes :: ComputationType -> Maybe SubFunctionSpecProvider
stdFunctionTypes (Conditional _ th el) = Just $ \i ar -> map (,ar) $ catMaybes [fmap (const $ fromString $ "thenFn" ++ show i) th, fmap (const $ fromString $ "elseFn" ++ show i) el]
stdFunctionTypes (Custom "map") = Just $ \i _ -> [(fromString $ "mapFn" ++ show i, 1)]
stdFunctionTypes _ = Nothing


genExampleBenchmark :: Int -> LGCmdArgs -> [(String, Serialized)] -- Name, Code
genExampleBenchmark
  seed
  lgArgs@(LGCmdArgs
           { totalGraphs = total
           , language = lang
           , slowdatasource = slowDS
           , cachenum = cache
           , Main.percentages = p
           })
  = concatenateTests (toCodeWrapped lang) $ snd $ mapAccumL (\x y -> swap $ randomBenchmark x y) stdGen lvllist
  where
    config = GeneratorConfig
      { LG.percentages = map (second (% (sum (map snd p)))) p
      , functionNodes = stdFunctionTypes
      , includeBigDataSource = slowDS
      }
    lvls = levels lgArgs - 1
    stdGen = System.Random.mkStdGen seed

    -- Derivated data structures
    lvllist = take total $ cycle [lvls,(lvls-1)..0]
    weightMap = exampleMapUpTo lvls
    randomBenchmark' :: (ReadConf m, MonadRandom m) => Int -> m NestedCodeGraph
    randomBenchmark' = randomExampleBenchmark weightMap

    randomBenchmark :: System.Random.StdGen -> Int -> (NestedCodeGraph, System.Random.StdGen)
    randomBenchmark gen lvl = flip runRand gen $ flip runReaderT config $
      case cache of
        Just c -> randomBenchmark' lvl >>= makeNestedCodeGraphRandomlyTimed c
        _      -> evalStateT (randomBenchmark' lvl) 0
        -- case lang of
        --                "HaskellDo" -> (\x y -> concatenateTests x y ++ "\nallTests :: [((Env u -> IO Int),Int,Int)]\nallTests = " ++ listTests y)
        --                "HaskellDoApp" -> (\x y -> concatenateTests x y ++ "\nallTests :: [((Env u -> IO Int),Int,Int)]\nallTests = " ++ listTests y) -- The same as HaskellDo
        --                _ -> concatenateTests


--  graphs <- Control.Monad.Random.evalRandIO singleString
--  putStrLn graphs


------------------------------------------------------------
-- Command-Line Arguments Parsing
------------------------------------------------------------

data LGCmdArgs = LGCmdArgs
  { output         :: Maybe String
  , levels         :: Int
  , totalGraphs    :: Int
  , language       :: String
  , seed           :: Maybe Int
  , maxDepth       :: Int
  , percentages    :: [(ComputationType, Integer)]
  , slowdatasource :: Bool
  , cachenum       :: Maybe Int
  } deriving Show

optparser :: ParserInfo LGCmdArgs
optparser = info
              (p <**> helper)
              (  fullDesc
              <> progDesc "Generate random programs"
              <> header "level-graphs")
  where
    langs = Map.keys acceptedLanguages
    langsStr' = intercalate ", " langs
    isPositiveInt thing i | i < 0 = Left $ thing ++ " must be positive"
                          | otherwise = Right i
    positiveIntReader thing = eitherReader $ isPositiveInt thing . read
    p = LGCmdArgs
          <$> option (Just <$> str)
              (  short 'o'
              <> long "output"
              <> metavar "PATH"
              <> value Nothing
              <> help "Output to file (prefix). \
                      \If no output file nor a namespace is given, only the graph code is output to stdout.")
          <*> option auto
              (  short 'l'
              <> long "levels"
              <> value 10
              <> showDefault
              <> metavar "INT"
              <> help "When several graphs are generated, this gives the maximum number of levels the graphs will have. \
                      \Will generate graphs having l levels, l-1 levels, and so forth until 1. \
                      \They will be repeated once the list is finished, if there are more graphs than levels.")
          <*> option auto
              (  short 'n'
              <> long "count"
              <> value 1
              <> showDefault
              <> metavar "INT"
              <> help "Total number of graphs to generate.")
          <*> option (eitherReader
                       $ \s -> if s `elem` langs then
                                 Right s
                               else
                                 Left $ "Language must be one of: " ++ langsStr' ++ "." )
              (  long "language"
              <> short 'L'
              <> help ("Language of the generated programs. Supported languages: " ++ langsStr' ++ ".")
              <> value "Ohua"
              <> showDefault
              <> completer (listCompleter langs))
          <*> option (Just <$> positiveIntReader "Seed")
              (  long "seed"
              <> short 's'
              <> value Nothing
              <> metavar "INT"
              <> help "Random seed for ensuring reproducibility (positive integer)")
          <*> option auto
              (  long "depth"
              <> short 'd'
              <> metavar "INT"
              <> help "Maximum nesting depth for functions in generated graphs."
              <> value 2
              <> showDefault)
          <*> some
              -- TODO add proper verification and error reporting
              (option (maybeReader
                        $ \s -> let (s',':':r) = break (== ':') s
                                in Just (fromString s', read r))
                (  long "percentage"
                <> short 'p'
                <> metavar "TYPE:VALUE"
                <> help "Setting probabilities for nodes. \
                        \Syntax -p=type:value where <type> is a node type and <value> is a measure for its relative likelyhood"))
          <*> switch
              (  short 'S'
              <> long "slow-datasource"
              <> help "Include a slow data source at one side.")
          <*> option (Just <$> positiveIntReader "Cache number")
              (  short 'c'
              <> long "cacheable"
              <> value Nothing
              <> metavar "INT"
              <> help "Make results that are cachable. \
                      \Will generate from c possible requests. \
                      \If flag is not present, caching is off. \
                      \In this case all requests are different.")

-- ----------------
--      main
-- ----------------
main :: IO ()
main = do
  lgArgs <- execParser optparser
        -- Main execution branch
  randSeed <- case seed lgArgs of
                Nothing -> do
                  randSeed <- fst . System.Random.random <$> System.Random.getStdGen
                  putStrLn $ "No seed provided, running with random seed: " ++ show randSeed
                  return randSeed
                Just s -> return s
      -- Setup (seed, output file)
      -- setSeed (seed lgArgs)
  let outputFile = output lgArgs
  print lgArgs
      -- Execute benchmark
  let outputStrings = genExampleBenchmark randSeed lgArgs

      -- Print it accordingly
      -- outputStrings <- case preamble lgArgs of
      --                    Nothing -> return outputStrings
      --                    Just file -> prepareOutputStrings file outputStrings

      -- if outputFile == "" then
  mapM_ print outputStrings
      -- else
      --    writeToFiles outputFile (fst $ fromJust $ Map.lookup (language lgArgs) acceptedLanguages) outputStrings
