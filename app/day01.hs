module Main where

import Criterion.Main
import Data.List (sort)
import qualified Data.Map as Map
import Utils (readInputLines)

parseLine :: String -> (Int, Int)
parseLine = (\[a, b] -> (a, b)) . map read . words

part1 :: [String] -> Int
part1 input =
  sum $ map abs (zipWith (-) (sort xs) (sort ys))
  where
    (xs, ys) = unzip $ map parseLine input

part2 :: [String] -> Int
part2 input =
  sum $ [x * Map.findWithDefault 0 x freq | x <- xs]
  where
    (xs, ys) = unzip $ map parseLine input
    freq = Map.fromListWith (+) [(y, 1) | y <- ys]

main :: IO ()
main = do
  input <- readInputLines 1
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 1"
        [ bench "part 1" $ nf part1 input,
          bench "part 2" $ nf part2 input
        ]
    ]
