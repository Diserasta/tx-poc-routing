module Mapper (
    Node,
    Ring,
    DistanceEntry,
    DistanceMatrix,
    makeRing,
    makeRingIO,
    genRing1Impl,
    followPath,
    followPathNoSan,
    cullList,
    cullPaths
) where

import qualified Helper as H
import Data.List (partition, elemIndex,minimumBy,(\\),deleteFirstsBy)
import Data.Ord (comparing)
import Data.Maybe (catMaybes)
import Debug.Trace

--Type definitions
--data Node = Node { id :: Int, poc :: Int } deriving (Show,Read,Eq,Ord)
type Node = Int
type Ring = [Node] -- A list of nodes by ID
--type DistanceEntry = (Node,Node,Double)
data DistanceEntry = DistanceEntry {
    source :: !Node,
    destination :: !Node,
    distance :: !Double
} deriving (Ord,Eq,Show,Read)

type DistanceMatrix = [DistanceEntry]

--Rules
-- POC 1 routers are always connected to other POC1s. No need to map this.
-- POC 2 Rings must be <= 4 devices (not including POC1 termination points)
-- POC 3 Rings should terminate on the same POC 2 Ring
-- POC 3 Rings should terminate on adjacent POC 2 routers where possible
-- POC 3 Rings must be <= 10 devices (not including POC 2 termination points)
-- POC 4 Rings have no adjacency requirements, but should terminate on the same POC 3 ring
-- POC 4 Rings must be <= 10 devices (not including POC 3 termination points)

--Assumptions
-- There exists some master list of nodes

-- getNodeID :: Node -> Int
-- getNodeID (Node id _) = id

-- getNodePOC :: Node -> Int
-- getNodePOC (Node _ poc) = poc

--Technical implementation of a path-follower
--Takes a tuple of nodes and costs, a src, a dest
pathfImpl :: Eq a => ([a],Double) -> a -> a -> [(a,a,Double)] -> [([a], Double)]
pathfImpl (trail, cost) src dest clauses
  | src == dest = [(src:trail, cost)]
  | otherwise = do
    let (nexts, rest) = partition ((==src) . H.get1st) clauses
    next <- nexts
    pathfImpl ((src:trail), cost + H.get3rd next) (H.get2nd next) dest rest

--Reverse all the directions as the path-follwer prepends to the trail so it can run in constant time
sanPath :: ([a], Double) -> ([a], Double)
sanPath (list, cost) = (reverse list, cost)

--Taking a source and destination point, as well as a list of links (and their costs)
--return a list of all acyclic paths from src to dest and their costs
followPath :: Eq a => a -> a -> [(a,a,Double)] -> [([a], Double)]
followPath src dest clauses = map sanPath (pathfImpl ([],0.0) src dest clauses)

--If we don't care about reversing
followPathNoSan :: Eq a => a -> a -> [(a,a,Double)] -> [([a], Double)]
followPathNoSan src dest clauses = pathfImpl ([],0.0) src dest clauses

--Get all lists greater than a size
cullList :: [[a]] -> Int -> [[a]]
cullList x k
  | k < 0 = error "cullList: length must be >= 0"
  | otherwise = filter (\l -> length l >= k) x

cullPaths :: Eq a => [([a],b)] -> Int -> [([a],b)]
cullPaths x k = do
    let (l,c) = unzip x -- ([[a]],[b])
    let nl = cullList l k -- [[a]]
    let nc = map (\o -> c !! o) $ catMaybes $ map (\a -> elemIndex a l) nl
    zip nl nc


makeRingIO :: (Node,Node) -> [(Node,Node,Double)] -> Int -> IO [Node]
makeRingIO (src,dest) tx len = do
    return (makeRing (src,dest) tx len)

--using all acyclic paths, grab the shortest walks under len hops
-- We know there's only 2 POC 1 sites, and they're connected, so always start at one and end at the other
-- Additionally, since we always know start and end point, we can do this one ring at a time
makeRing :: (Node,Node) -> [(Node,Node,Double)] -> Int -> [Node]
makeRing (src,dest) tx len = do
    let (l,c) = unzip $ cullPaths (followPathNoSan src dest tx) len -- ([[a]],[b])
    --traceShowM (l,c)
    let (_,best) = minimumBy (comparing fst) (zip c [0..]) -- find the index of the smallest cost
    l !! best

--Since we can make a ring, we now need to make all rings for POC 1 -> POC 2
--We can do this by supplying a list of all nodes that must be connected
genRing1Impl :: (Node, Node) -> [(Node,Node,Double)] -> [Node] -> Int -> [[Node]] -> [[Node]]
genRing1Impl (src,dest) tx avail len trail
  | avail == [] = trail
  | (head trail) == src:dest:[] = trail
  | length avail < len = genRing1Impl (src,dest) tx avail (length avail) trail
  | otherwise = do
      --traceShowM tx
      --traceShowM len
      let r = (makeRing (src,dest) tx (len + 2))
      --traceShowM r
      --traceShowM (avail)
      let tx' = filter (\z -> notElem (H.get1st z) (tail $ init r) ) tx
      --traceShowM trail
      genRing1Impl (src,dest) (tx') (avail \\ r) len (r:trail)