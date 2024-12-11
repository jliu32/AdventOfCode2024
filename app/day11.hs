module Main where

import Control.Monad.State
import Criterion.Main
import Data.Map (Map)
import qualified Data.Map as Map
import Utils (readInputLines)

parseLine :: String -> [Int]
parseLine = map read . words

transform :: Int -> [Int]
transform s
  | s == 0 = [1]
  | even (length $ show s) = let (l, r) = splitAt (length (show s) `div` 2) (show s) in [read l, read r]
  | otherwise = [s * 2024]

lengthMemo :: Int -> Int -> State (Map (Int, Int) Int) Int
lengthMemo n s
  | n == 0 = return 1
  | otherwise = do
      cache <- get
      case Map.lookup (n, s) cache of
        Just val -> return val
        Nothing -> do
          let next = transform s
          lengths <- mapM (lengthMemo (n - 1)) next
          let total = sum lengths
          modify' (Map.insert (n, s) total)
          return total

lengthN :: Int -> [Int] -> Int
lengthN n xs =
  evalState (fmap sum (mapM (lengthMemo n) xs)) Map.empty

part1 :: [String] -> Int
part1 input = lengthN 25 (parseLine $ head input)

part2 :: [String] -> Int
part2 input = lengthN 75 (parseLine $ head input)

main :: IO ()
main = do
  input <- readInputLines 11
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 11"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
