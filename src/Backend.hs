{-# LANGUAGE OverloadedStrings #-}
module Backend where

import           LevelGraphs               (NestedCodeGraph)

import qualified Data.Graph.Inductive      as Graph

-- TODO: a more general mechanism is needed here that should take this configuration from the command line. -> API-design.
import           Backend.Language.Common
import           Backends.Haxl             (toHaxlAppCode, toHaxlCode)
import           Backends.Muse             (toMuseAppCode, toMuseCode)
import           Backends.Ohua             (toOhuaAppCodeWrapped,
                                            toOhuaCodeWrapped)
import qualified Data.Map                  as Map
import           Data.Text.Prettyprint.Doc

------------------------------------------------------------
-- General Backend
------------------------------------------------------------

type GraphGen a = String -> NestedCodeGraph -> Serialized
type FileExtension = String

acceptedLanguages :: Map.Map String (FileExtension, GraphGen a)
acceptedLanguages = Map.fromList
    [ ("Ohua", (".clj", toOhuaCodeWrapped))
    , ("OhuaApp", (".clj", toOhuaAppCodeWrapped))
    , ("HaxlDoApp", (".hs", toHaxlAppCode))
    , ("HaxlDo", (".hs", toHaxlCode))
    , ("MuseApp", (".clj", toMuseAppCode))
    , ("MuseMonad", (".clj", toMuseCode))
    , ("Graph", ("", toGraphCodeWrapped))
    ]

-- Improve: replace "string" with a good language type
toCodeWrapped :: String -> String -> NestedCodeGraph -> Serialized
toCodeWrapped = maybe (error "Unexpected language case error") snd . flip Map.lookup acceptedLanguages


------------------------------------------------------------
-- Graph Backend (pretty print of graph)
------------------------------------------------------------

toGraphCodeWrapped :: String -> NestedCodeGraph -> Serialized
toGraphCodeWrapped name (graph, subgraphs) =
  vcat $
    [ "Graph-" <> pretty name
    , pretty $ Graph.prettify graph
    ] ++
    (map (\(subgraph,_, _) -> "Subgraph-" <> pretty name <> line <> pretty (Graph.prettify subgraph)) subgraphs)
