module Main where

import Criterion.Main
import Data.Array
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Utils (readInputLines)

type Coord = (Int, Int)
type Dir = Coord
type State = (Coord, Dir)
type Maze = Array Coord Char

parseInput :: [String] -> Maze
parseInput input = listArray ((0, 0), (h - 1, w - 1)) (concat input)
 where
  h = length input
  w = length (head input)

directions :: [Dir]
directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

turnLeft, turnRight :: Dir -> Dir
turnLeft (dx, dy) = (-dy, dx)
turnRight (dx, dy) = (dy, -dx)

move :: Coord -> Dir -> Coord
move (x, y) (dx, dy) = (x + dx, y + dy)

isValidMove :: Maze -> Coord -> Bool
isValidMove maze pos =
  let ((minX, minY), (maxX, maxY)) = bounds maze
   in x >= minX && x <= maxX && y >= minY && y <= maxY && (maze ! pos == '.' || maze ! pos == 'E')
 where
  (x, y) = pos

start :: Maze -> Coord
start maze = head [pos | (pos, c) <- assocs maze, c == 'S']

search :: Maze -> Set State -> IntMap (Map State (Set Coord)) -> (Int, Int)
search maze seen queue =
  case IM.minViewWithKey queue of
    Nothing -> error "No solution found"
    Just ((cost, current), restQueue)
      | not (null ends) -> (cost, length (mconcat ends))
      | otherwise -> search maze seen' newQueue
     where
      ends = [b | ((e, _), b) <- M.assocs newStates, maze ! e == 'E']
      seen' =
        S.union seen $
          S.fromList (M.keys newStates)
      newStates = M.withoutKeys current seen
      newQueue =
        IM.unionWith merge restQueue $
          IM.fromListWith
            merge
            [ next
            | ((p, v), path) <- M.assocs newStates
            , next <-
                [(cost + 1000, M.singleton (p, turnRight v) path)]
                  ++ [(cost + 1000, M.singleton (p, turnLeft v) path)]
                  ++ [(cost + 1, M.singleton (move p v, v) (S.insert (move p v) path)) | isValidMove maze (move p v)]
            ]
      merge = M.unionWith S.union

part1 :: [String] -> Int
part1 input = fst $ search maze S.empty initQ
 where
  maze = parseInput input
  s = start maze
  initD = (0, 1)
  initQ = IM.singleton 0 (M.singleton (s, initD) (S.singleton s))

part2 :: [String] -> Int
part2 input = snd $ search maze S.empty initQ
 where
  maze = parseInput input
  s = start maze
  initD = (0, 1)
  initQ = IM.singleton 0 (M.singleton (s, initD) (S.singleton s))

main :: IO ()
main = do
  input <- readInputLines 16
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 16"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
