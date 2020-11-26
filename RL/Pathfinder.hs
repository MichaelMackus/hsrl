module RL.Pathfinder where

import Control.Monad.State
import Data.Set (Set)
import Data.PQueue.Min (MinQueue)
import qualified Data.Set as Set
import qualified Data.PQueue.Min as PQ

-- Modified from https://hackage.haskell.org/package/astar-0.3.0.0/docs/Data-Graph-AStar.html
--
-- In this case "a" would be "Tile". We could likely do away with the
-- distance function for now.
--
-- findPath :: (Hashable a, Ord a, Ord c, Num c)
-- => (a -> Set a)	    -- The graph we are searching through, given as a function from vertices to their neighbours.
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

findPath :: (Ord a, Ord c, Num c, Show a, Show c)
         => (a -> Set a)     -- Tile -> Neighbors
         -> (a -> a -> c)  -- heuristic distance between tiles
         -> a              -- end
         -> a              -- start
         -> Maybe [a]
findPath f h end start
        = evalState (findPathM (finder f h (mkNode end)) h (mkNode end) (mkNode start)) $ AStar mempty mempty
    where zero     = h start start
          mkNode p = ANode zero p Nothing

-- find optimal path using priority queue
findPathM :: (Monad m, Ord a, Ord c, Num c, Show a, Show c)
         => (ANode a c -> m (Maybe (ANode a c))) -- Tile -> Neighbors
         -> (a -> a -> c)                       -- heuristic distance between tiles
         -> ANode a c                           -- end
         -> ANode a c                           -- start
         -> m (Maybe [a])
findPathM f h end start
    | val end == val start = return . Just . reverse . nodeToList $ start
    | otherwise    = do
        next <- f start
        case next of
            Just next -> findPathM f h end next
            otherwise -> return Nothing

data ANode a c = ANode { score :: c, val :: a, parent :: Maybe (ANode a c) }
instance Ord c => Ord (ANode a c) where
    compare n n' = compare (score n) (score n')
instance Eq c => Eq (ANode a c) where
    n == n' = score n == score n'
instance (Show a, Show c) => Show (ANode a c) where
    show (ANode s v _) = "Node { s = " ++ show s ++ ", v = " ++ show v ++ " }"

nodeToList :: ANode a c -> [a]
nodeToList (ANode _ v Nothing)  = [v]
nodeToList (ANode _ v (Just n)) = v:nodeToList n

data AStar a c = AStar { queue :: MinQueue (ANode a c), visited :: Set a }

-- simple finder implementation using priority queue
finder :: (Ord a, Ord c, Num c, Show a, Show c)
    => (a -> Set a)
    -> (a -> a -> c)
    -> ANode a c
    -> ANode a c
    -> State (AStar a c) (Maybe (ANode a c))
finder f h end node = do
        queue <- gets queue
        vis   <- gets visited
        let neighbors = Set.filter (not . (`Set.member` vis)) $ f (val node)
            queue'    = PQ.union (PQ.fromList (map mkNode (Set.toList neighbors))) queue
            next      = PQ.getMin queue'
        put $ AStar (PQ.deleteMin queue') (Set.union neighbors vis)
        return next
    where mkNode    v = ANode (calcScore v) v (Just node)
          calcScore v = score node + h v (val end) + h (val node) v
