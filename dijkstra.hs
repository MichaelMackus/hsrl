import RL.Pathfinder

import qualified Data.Map as M

import Graph.DijkstraSimple
import Graph.DijkstraSimple.Weighters

exampleGraph :: Graph Char Int
exampleGraph = Graph $ M.fromList [
                                    ('A', [EdgeTo 'B' 3, EdgeTo 'C' 1])
                                  , ('B', [EdgeTo 'A' 3, EdgeTo 'C' 7, EdgeTo 'D' 5, EdgeTo 'E' 1])
                                  , ('C', [EdgeTo 'A' 1, EdgeTo 'B' 7, EdgeTo 'D' 2])
                                  , ('D', [EdgeTo 'B' 5, EdgeTo 'C' 2, EdgeTo 'E' 5])
                                  , ('E', [EdgeTo 'B' 1, EdgeTo 'D' 7])
                                  ]

main = do
    print $ lightestPaths exampleGraph 'C' cumulativeWeighter
