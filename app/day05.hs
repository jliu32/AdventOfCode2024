{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.List
import Data.List.Split
import Utils (readInputLines)

data Page = Page [(Int, Int)] Int
  deriving (Eq)

instance Ord Page where
  (Page r1 a) `compare` (Page _ b) =
    if (a, b) `elem` r1 then LT else GT

instance Show Page where
  show (Page _ a) = show a

sorted :: (Ord a) => [a] -> Bool
sorted xs = and $ zipWith (<) xs (tail xs)

parseRule :: [String] -> [(Int, Int)]
parseRule =  map ((\[a, b] -> (a, b)) . map read . splitOn "|")

parseUpdate :: [String] -> [[Int]]
parseUpdate =  map (map read . splitOn ",")

correctOrder :: [Page] -> Int
correctOrder xs = if sorted xs then half else 0
  where
    Page _ half = xs !! (length xs `div` 2)

part1 :: [String] -> Int
part1 input = sum $ map (correctOrder . toPages) updates
  where
    rules = parseRule $ takeWhile (/= "") input
    updates = parseUpdate $ tail $ dropWhile (/= "") input
    ordering xs = [(a, b) | (a, b) <- rules, a `elem` xs && b `elem` xs]
    toPages xs = map (Page (ordering xs)) xs

fixOrder :: [Page] -> Int
fixOrder xs = if sorted xs then 0 else half
  where
    Page _ half = sort xs !! (length xs `div` 2)

part2 :: [String] -> Int
part2 input = sum $ map (fixOrder . toPages) updates
  where
    rules = parseRule $ takeWhile (/= "") input
    updates = parseUpdate $ tail $ dropWhile (/= "") input
    ordering xs = [(a, b) | (a, b) <- rules, a `elem` xs && b `elem` xs]
    toPages xs = map (Page (ordering xs)) xs

main :: IO ()
main = do
  input <- readInputLines 5
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 4"
        [ bench "part 1" $ nf part1 input,
          bench "part 2" $ nf part2 input
        ]
    ]
