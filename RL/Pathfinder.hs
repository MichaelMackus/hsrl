module RL.Pathfinder where

import RL.Types (Point, distance)

import Control.Monad.State
import Data.List.NonEmpty (toList)
import Data.Set (Set)
import Data.Map (Map)
import Data.PQueue.Min (MinQueue)
import qualified Data.Set as Set
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.PQueue.Min as PQ

import Graph.DijkstraSimple.Weighters (cumulativeWeighter)
import qualified Graph.DijkstraSimple as G

-- findPath uses the Dijkstra algorithm to find a path from start to end
findPath :: (Point -> [Point])  -- neighborF, returns neighbors of a tile
         -> Point               -- start
         -> Point               -- end
         -> Maybe [Point]
findPath neighborF start end = let graph = toGraph map neighborF
                                   path  = G.findPath graph start defaultWeighter end
                                   map   = toPoints neighborF start
                               in  snd . fromPath <$> path

-- TODO new function to find lightestPaths from point, e.g. for
-- retreating we set start point to player and return the path with the
-- highest weight (via "lightestPaths" fn)
--
-- find all paths from start point
findPathsFrom :: (Point -> [Point]) -- neighborF, returns neighbors of a tile
              -> Point              -- start
              -> [(Int, [Point])]
findPathsFrom neighborF start = let graph = toGraph map neighborF
                                    paths = G.lightestPaths graph start defaultWeighter
                                    map   = toPoints neighborF start
                                in  fromPaths paths

toPoints :: (Point -> [Point]) -> Point -> [Point]
toPoints neighbors start = Set.toList (expand Set.empty [start])
    where expand s (p:ps) = let ps' = neighbors p
                                s'  = s <> Set.fromList ps'
                                f p = not (p `Set.member` s)
                            in  expand s' (L.filter f ps' ++ ps)
          expand s []     = s

toGraph :: (Ord a, Num a) => [Point] -> (Point -> [Point]) -> G.Graph Point a
toGraph ps edges = G.Graph $ M.fromList (map f ps)
    where f p = (p, map h (edges p))
          h p = G.EdgeTo p (fromIntegral 1)

fromPaths :: (Ord a, Num a, Ord b, Num b) => G.Paths Point a b -> [(b, [Point])]
fromPaths = map fromPath . M.elems . G.pathsAsMap

fromPath :: (Ord a, Num a, Ord b, Num b) => G.Path Point a b -> (b, [Point])
fromPath p = (G.pathWeight p, reverse . toList . G.pathVertices $ p)

defaultWeighter = cumulativeWeighter
-- defaultWeighter = G.Weighter 0 f
--     where f (G.EdgeTo p w) (G.Path vs w') = distance p (head (toList vs))

-- toWeighter :: (Ord a, Num a)
--          => (Point -> Point -> a)
--          -> Weighter Point Int a
-- toWeighter h = Weighter (fromIntegral 1) f
--     where f (EdgeTo p n) (Path p n kkk
