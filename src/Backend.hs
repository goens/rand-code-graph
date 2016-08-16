module Backend where

import           LevelGraphs          (FunctionMeta (..), NestedCodeGraph)

import qualified Data.Graph.Inductive as Graph

-- TODO: a more general mechanism is needed here that should take this configuration from the command line. -> API-design.
import qualified Backends.Common
import           Backends.Haxl        (toHaxlDoAppCodeWrapped,
                                       toHaxlDoCodeWrapped)
import           Backends.Muse        (toMuseAppCodeWrapped,
                                       toMuseMonadCodeWrapped)
import           Backends.Ohua        (toOhuaAppCodeWrapped, toOhuaCodeWrapped)
import qualified Data.Map             as Map

------------------------------------------------------------
-- General Backend
------------------------------------------------------------

type GraphGen = String -> NestedCodeGraph -> String
type FileExtension = String

acceptedLanguages :: Map.Map String (FileExtension, GraphGen)
acceptedLanguages = Map.fromList
    [ ("Ohua", (".clj", toOhuaCodeWrapped))
    , ("OhuaApp", (".clj", toOhuaAppCodeWrapped))
    , ("HaxlDoApp", (".hs", toHaxlDoAppCodeWrapped))
    , ("HaxlDo", (".hs", toHaxlDoCodeWrapped))
    , ("MuseApp", (".clj", toMuseAppCodeWrapped))
    , ("MuseMonad", (".clj", toMuseMonadCodeWrapped))
    , ("Graph", ("", toGraphCodeWrapped))
    ]

-- Improve: replace "string" with a good language type
toCodeWrapped :: String -> String -> NestedCodeGraph -> String
toCodeWrapped = maybe (\_ _ -> "Unexpected language case error") snd . flip Map.lookup acceptedLanguages


------------------------------------------------------------
-- Graph Backend (pretty print of graph)
------------------------------------------------------------

toGraphCodeWrapped :: String -> NestedCodeGraph -> String
toGraphCodeWrapped name (graph, subgraphs) =
    "Graph-" ++ name ++ "\n" ++ Graph.prettify graph ++ "\n" ++
    (concatMap (\FunctionMeta{fn=subgraph} -> "Subgraph-" ++ name ++ "\n" ++ Graph.prettify subgraph ++ "\n") subgraphs)


inlineIfBranches = Backends.Common.inlineIfBranches
