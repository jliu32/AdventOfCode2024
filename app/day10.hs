module Main where

import Control.Monad
import Criterion.Main
import Data.Array
import Data.List (nub)
import Utils (readInputLines)

newtype Topomap = Topomap {cells :: Array Pos Cell}
  deriving (Show)

type Pos = (Int, Int)

newtype Cell = Cell {height :: Int}
  deriving (Show)

type Trail = [Pos]

parseInput :: [String] -> Topomap
parseInput input = Topomap topo
 where
  h = length input
  w = length (head input)
  topo =
    array
      ((0, 0), (h - 1, w - 1))
      [ ((r, c), Cell $ read [ch])
      | (r, row) <- zip [0 ..] input
      , (c, ch) <- zip [0 ..] row
      ]

trailheads :: Topomap -> [Pos]
trailheads m = map fst $ filter (\x -> height (snd x) == 0) $ assocs (cells m)

score :: Bool -> Topomap -> Pos -> Int
score isRating m th = if isRating then length $ search th else length . nub $ map last $ search th
 where
  ((minCol, minRow), (maxCol, maxRow)) = bounds $ cells m

  search current
    | height (cells m ! current) == 9 = return [current]
    | otherwise = do
        nbr <- neighbour current
        pathFromNbr <- search nbr
        return $ current : pathFromNbr

  neighbour current@(x, y) = do
    (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]
    let x' = x + dx
        y' = y + dy
        nbr = (x', y')
        nbrh = height $ cells m ! nbr
    guard (x' >= minCol && x' <= maxCol)
    guard (y' >= minRow && y' <= maxRow)
    guard (nbrh - height (cells m ! current) == 1)
    return nbr

part1 :: [String] -> Int
part1 input =
  let tm = parseInput input
   in sum $ map (score False tm) $ trailheads tm

part2 :: [String] -> Int
part2 input =
  let tm = parseInput input
   in sum $ map (score True tm) $ trailheads tm

main :: IO ()
main = do
  input <- readInputLines 10
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 10"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
