module Main where

import Control.Monad
import Criterion.Main
import Data.List (tails)
import Data.Map as M
import Data.Set as S
import Utils (readInputLines)

parseInput :: [String] -> M.Map Char [(Int, Int)]
parseInput input = M.fromListWith (++) [(ch, [(r, c)]) | (r, row) <- zip [0 ..] input, (c, ch) <- zip [0 ..] row, ch /= '.']

signal :: Bool -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
signal isHarmonics h w ants = do
  (a1, a2) <- [(x, y) | (x : ys) <- tails ants, y <- ys]
  (x, y) <- antinodes a1 a2
  guard (x >= 0 && x < h)
  guard (y >= 0 && y < w)
  return (x, y)
 where
  range = if isHarmonics then [-(max h w) .. max h w] else [-1, 2]
  antinodes (ax1, ay1) (ax2, ay2) =
    [(ax1 + n * (ax2 - ax1), ay1 + n * (ay2 - ay1)) | n <- range]

part1 :: [String] -> Int
part1 input = S.size antinodes
 where
  h = length input
  w = length $ head input
  antMap = parseInput input
  antinodes = S.fromList $ concat . M.elems $ M.map (signal False h w) antMap

part2 :: [String] -> Int
part2 input = S.size antinodes
 where
  h = length input
  w = length $ head input
  antMap = parseInput input
  antinodes = S.fromList $ concat . M.elems $ M.map (signal True h w) antMap

main :: IO ()
main = do
  input <- readInputLines 8
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 1"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
