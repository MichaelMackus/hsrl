module RL.Pathfinder where

import RL.Types (Point, distance)

import Control.Monad.State
import Data.List.NonEmpty (toList)
import Data.Set (Set)
import Data.Map (Map)
import Data.PQueue.Min (MinQueue)
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.PQueue.Min as PQ

import Graph.DijkstraSimple.Weighters (cumulativeWeighter)
import qualified Graph.DijkstraSimple as G

-- findPath uses the Dijkstra algorithm to find a path from start to end
findPath :: (Point -> [Point])     -- neighborF, returns neighbors of a tile
         -> Point                  -- start
         -> Point                  -- end
         -> Maybe [Point]
findPath neighborF start end = let graph = toGraph (toPoints neighborF start) neighborF
                                   path  = G.findPath graph start defaultWeighter end
                               in  fromPath <$> path

-- TODO new function to find lightestPaths from point, e.g. for
-- retreating we set start point to player and return the path with the
-- highest weight (via "lightestPaths" fn)

toPoints :: (Point -> [Point]) -> Point -> [Point]
toPoints neighbors start = Set.toList (expand Set.empty [start])
    where expand s (p:ps) = if p `Set.member` s then expand s ps
                            else let s'  = Set.insert p s
                                     ps' = neighbors p
                                 in  expand s' (ps' ++ ps)
          expand s []     = s
          -- f p ps = if p `Set.member` ps then ps
          --          else foldr Set.insert ps (f p)

toGraph :: (Ord a, Num a) => [Point] -> (Point -> [Point]) -> G.Graph Point a
toGraph ps edges = G.Graph $ M.fromList (map f ps)
    where f p = (p, map h (edges p))
          h p = G.EdgeTo p (fromIntegral 1)

defaultWeighter = cumulativeWeighter
-- defaultWeighter = G.Weighter 0 f
--     where f (G.EdgeTo p w) (G.Path vs w') = distance p (head (toList vs))

fromPath :: (Ord a, Num a, Ord b, Num b) => G.Path Point a b -> [Point]
fromPath = reverse . toList . G.pathVertices

-- toWeighter :: (Ord a, Num a)
--          => (Point -> Point -> a)
--          -> Weighter Point Int a
-- toWeighter h = Weighter (fromIntegral 1) f
--     where f (EdgeTo p n) (Path p n kkk
