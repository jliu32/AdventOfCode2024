module Main where

import Criterion.Main
import Data.List (isPrefixOf)
import Utils (readInputLines)

rows :: Int
rows = 140

columns :: Int
columns = 140

check :: [String] -> String -> (Int, Int) -> (Int, Int) -> Bool
check matrix word (sr, sc) (rd, cd) =
  let positions = [(sr + i * rd, sc + i * cd) | i <- [0 .. length word - 1]]
      chars = [matrix !! r !! c | (r, c) <- positions, r >= 0, c >= 0, r < rows, c < columns]
   in word `isPrefixOf` chars

findall :: [String] -> String -> Int
findall matrix word =
  sum $ [fromEnum (check matrix word (r, c) dir) | r <- [0 .. rows - 1], c <- [0 .. columns - 1], dir <- directions]
  where
    directions = [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]

part1 :: [String] -> Int
part1 input = findall input "XMAS"

checkxmas :: [String] -> Int
checkxmas matrix =
  let positions = [(ar, ac) | (ar, row) <- zip [0 ..] matrix, (ac, char) <- zip [0 ..] row, char == 'A']
   in sum $ map (fromEnum . ismax) positions
  where
    ismax (r, c) =
      any
        ( \(d1, d2) ->
            let (d1r, d1c) = (r + d1, c + d1)
                (d2r, d2c) = (r + d2, c - d2)
                (d3r, d3c) = (r - d1, c - d1)
                (d4r, d4c) = (r - d2, c + d2)
             in d1r >= 0
                  && d1c >= 0
                  && d2r >= 0
                  && d2c >= 0
                  && d3r >= 0
                  && d3c >= 0
                  && d4r >= 0
                  && d4c >= 0
                  && d1r < rows
                  && d1c < columns
                  && d2r < rows
                  && d2c < columns
                  && d3r < rows
                  && d3c < columns
                  && d4r < rows
                  && d4c < columns
                  && matrix !! d1r !! d1c == 'M'
                  && matrix !! d3r !! d3c == 'S'
                  && matrix !! d2r !! d2c == 'M'
                  && matrix !! d4r !! d4c == 'S'
        )
        [(-1, -1), (-1, 1), (1, -1), (1, 1)]

part2 :: [String] -> Int
part2 = checkxmas

main :: IO ()
main = do
  input <- readInputLines 4
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 4"
        [ bench "part 1" $ nf part1 input,
          bench "part 2" $ nf part2 input
        ]
    ]
