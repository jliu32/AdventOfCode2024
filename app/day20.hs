module Main where

import Criterion.Main
import Data.Array.Unboxed
import Data.List (sort, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Utils (readInputLines)

type Coord = (Int, Int)
type Grid = UArray Coord Bool
type Maze = UArray Coord Char

heuristic :: Coord -> Coord -> Int
heuristic (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

astar :: Grid -> Coord -> Coord -> [(Coord, Int)]
astar grid start goal = reconstructPath cameFrom goal
 where
  gScore = Map.singleton start 0
  openSet = [(start, heuristic start goal)]
  (cameFrom, gs) = search openSet gScore Map.empty
  search [] gs cameFrom = (cameFrom, gs)
  search openSet gScore cameFrom
    | current == goal = (cameFrom, gScore)
    | otherwise = search newOpenSet newGScore newCameFrom
   where
    (current, _) : rest = openSet
    neighbors = validNeighbors current grid
    currentG = fromMaybe maxBound (Map.lookup current gScore)
    (newOpenSet, newGScore, newCameFrom) = foldl update (rest, gScore, cameFrom) neighbors
    update (os, gs, cf) neighbor
      | tentativeG < fromMaybe maxBound (Map.lookup neighbor gs) =
          let newGScore = Map.insert neighbor tentativeG gs
              newF = tentativeG + heuristic neighbor goal
              newCameFrom = Map.insert neighbor current cf
              newOpenSet = insertWithPriority (neighbor, newF) os
           in (newOpenSet, newGScore, newCameFrom)
      | otherwise = (os, gs, cf)
     where
      tentativeG = currentG + 1
  reconstructPath cameFrom current
    | current == start = [(start, 0)]
    | otherwise = (current, gs Map.! current) : reconstructPath cameFrom (cameFrom Map.! current)

insertWithPriority :: (Coord, Int) -> [(Coord, Int)] -> [(Coord, Int)]
insertWithPriority x = sortOn snd . (x :)

validNeighbors :: Coord -> Grid -> [Coord]
validNeighbors (x, y) grid = filter isValid [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
 where
  isValid coord = inRange (bounds grid) coord && grid ! coord

parseInput :: [String] -> Maze
parseInput input = listArray ((0, 0), (h - 1, w - 1)) (concat input)
 where
  h = length input
  w = length (head input)

part1 :: [String] -> Int
part1 input =
  length
    [ ()
    | (p1, True) <- assocs grid
    , (p2, True) <- assocs grid
    , let d = heuristic p1 p2
    , d > 0
    , d <= 2
    , let c1 = optimals Map.! p1
    , let c2 = optimals Map.! p2
    , c1 - c2 - d >= 100
    ]
 where
  maze = parseInput input
  grid = amap (/= '#') maze
  start = head [p | (p, 'S') <- assocs maze]
  end = head [p | (p, 'E') <- assocs maze]
  optimals = Map.fromList (astar grid start end)

part2 :: [String] -> Int
part2 input =
  length
    [ ()
    | (p1, True) <- assocs grid
    , (p2, True) <- assocs grid
    , let d = heuristic p1 p2
    , d > 0
    , d <= 20
    , let c1 = optimals Map.! p1
    , let c2 = optimals Map.! p2
    , c1 - c2 - d >= 100
    ]
 where
  maze = parseInput input
  grid = amap (/= '#') maze
  start = head [p | (p, 'S') <- assocs maze]
  end = head [p | (p, 'E') <- assocs maze]
  optimals = Map.fromList (astar grid start end)

main :: IO ()
main = do
  input <- readInputLines 20
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 20"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
