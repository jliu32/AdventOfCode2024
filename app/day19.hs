module Main where

import Criterion.Main
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.MemoTrie (memoFix)
import Utils (readInputLines)

parseLine :: [String] -> ([String], [String])
parseLine input = (splitOn ", " patterns, designs)
 where
  patterns = head input
  designs = drop 2 input

display :: [String] -> String -> Bool
display patterns = memoFix (\f str -> go f patterns str)
 where
  go _ _ [] = True
  go f pats design = any (\p -> p `isPrefixOf` design && f (drop (length p) design)) pats

part1 :: [String] -> Int
part1 input = sum . map fromEnum $ map (\line -> display pats line) designs
 where
  (pats, designs) = parseLine input

allDisplay :: [String] -> String -> Int
allDisplay patterns =
  memoFix
    ( \f str ->
        if null str
          then 1
          else
            sum
              [ f (drop (length p) str)
              | p <- patterns
              , p `isPrefixOf` str
              ]
    )

part2 :: [String] -> Int
part2 input = sum ways
 where
  (pats, designs) = parseLine input
  ways = map (\line -> allDisplay pats line) designs

main :: IO ()
main = do
  input <- readInputLines 19
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 19"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
