module RL.Pathfinder where

import RL.Types

import Data.Maybe (listToMaybe, catMaybes)

-- Stolen from https://hackage.haskell.org/package/astar-0.3.0.0/docs/Data-Graph-AStar.html
--
-- In this case "a" would be "Tile". We could likely do away with the
-- distance function for now.
--
-- findPath :: (Hashable a, Ord a, Ord c, Num c)
-- => (a -> HashSet a)	-- The graph we are searching through, given as a function from vertices to their neighbours.
-- -> (a -> a -> c)	    -- Distance function between neighbouring
--                      -- vertices of the graph. This will never be applied to vertices that
--                      -- are not neighbours, so may be undefined on pairs that are not
--                      -- neighbours in the graph.
-- -> (a -> c)	        -- Heuristic distance to the (nearest) goal.
--                      -- This should never overestimate the distance, or else the path found
--                      -- may not be minimal.
-- -> (a -> Bool)	    -- The goal, specified as a boolean predicate on vertices.
-- -> a	                -- The vertex to start searching from.
-- -> Maybe [a]	        -- An optimal path, if any path exists. This excludes the starting vertex.


findPath :: (Point -> [Point]) -- Tile -> Neighbors
         -> Point              -- end
         -> Point              -- start
         -> Maybe [Point]
findPath f end start
    | end == start = pure [end]
    | otherwise    =
        let neighbors = f start
            rs = catMaybes (map (findPath f end) neighbors)
        in  (start:) <$> listToMaybe rs
