module Main where

import Control.Monad (replicateM)
import Criterion.Main
import Data.Char (isDigit)
import Utils (readInputLines)

parseLine :: String -> (Int, [Int])
parseLine l = let xs = words l in (read $ filter isDigit (head xs), map read (tail xs))

operators :: [Int -> Int -> Int]
operators = [(+), (*)]

insertOp :: Int -> [Int -> Int -> Int] -> [Int] -> [Int]
insertOp t ops nums =
  let n = length nums - 1
      combinations = replicateM n ops
   in map (eval t nums) combinations

eval :: Int -> [Int] -> [Int -> Int -> Int] -> Int
eval t (x : xs) ops = foldl ap x (zip ops xs)
 where
  ap acc (op, num) = if acc > t then 0 else acc `op` num

part1 :: [String] -> Int
part1 input = sum $ map (test . parseLine) input
 where
  test (r, eq) = if r `elem` insertOp r operators eq then r else 0

concatDigit :: Int -> Int -> Int
concatDigit a b = a * (10 ^ (floor (logBase 10 (fromIntegral b)) + 1)) + b

newOps :: [Int -> Int -> Int]
newOps = concatDigit : operators

part2 :: [String] -> Int
part2 input = sum $ map (test . parseLine) input
 where
  test (r, eq) = if r `elem` insertOp r newOps eq then r else 0

main :: IO ()
main = do
  input <- readInputLines 7
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 1"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
