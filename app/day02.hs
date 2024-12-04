module Main where

import Control.Applicative (liftA2)
import Criterion.Main
import Data.List (inits, tails)
import Utils (readInputLines)

parseLine :: String -> [Int]
parseLine = map read . words

safe :: [Int] -> Bool
safe xs = increse diff || decrese diff
  where
    diff = zipWith (-) xs (tail xs)
    increse = all ((> 0) <&&> (< 4))
    decrese = all ((< 0) <&&> (> -4))
    (<&&>) = liftA2 (&&)

dampener :: [Int] -> Bool
dampener xs = any safe xss
  where
    xss = zipWith (++) (init $ inits xs) (map tail . init $ tails xs)

part1 :: [String] -> Int
part1 input = sum $ map ((fromEnum . safe) . parseLine) input

part2 :: [String] -> Int
part2 input = sum $ map ((fromEnum . dampener) . parseLine) input

main :: IO ()
main = do
  input <- readInputLines 2
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 2"
        [ bench "part 1" $ nf part1 input,
          bench "part 2" $ nf part2 input
        ]
    ]
