module Backend where

import LevelGraphs (NestedCodeGraph)

import qualified Data.Graph.Inductive as Graph

-- TODO: a more general mechanism is needed here that should take this configuration from the command line. -> API-design.
import Backends.Ohua        (toOhuaAppCodeWrapped, toOhuaCodeWrapped)
import Backends.Haxl        (toHaskellDoCodeWrapped, toHaskellDoAppCodeWrapped)
import Backends.Muse        (toMuseMonadCodeWrapped, toMuseAppCodeWrapped)

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
-- Graph Backend (pretty print of graph)
------------------------------------------------------------

toGraphCodeWrapped :: String -> NestedCodeGraph -> String
toGraphCodeWrapped name (graph, subgraphs) =
    "Graph-" ++ name ++ "\n" ++ Graph.prettify graph ++ "\n" ++
    (concat $ flip map subgraphs (\(subgraph,_) -> "Subgraph-" ++ name ++ "\n" ++ Graph.prettify subgraph ++ "\n"))
