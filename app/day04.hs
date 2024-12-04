module Main where

import Criterion.Main
import Utils (readInputLines)

rows, columns :: Int
rows = 140
columns = 140

isValidPos :: [String] -> (Int, Int) -> Char -> Bool
isValidPos matrix (r, c) ch = r >= 0 && c >= 0 && r < rows && c < columns && matrix !! r !! c == ch

check :: [String] -> String -> (Int, Int) -> (Int, Int) -> Bool
check matrix word (sr, sc) (dr, dc) =
  all (\(i, ch) -> isValidPos matrix (sr + i * dr, sc + i * dc) ch) (zip [0 ..] word)

findall :: [String] -> String -> Int
findall matrix word =
  sum $
    [ fromEnum (check matrix word (r, c) dir)
      | (r, row) <- zip [0 ..] matrix,
        (c, char) <- zip [0 ..] row,
        char == head word,
        dir <- [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, 1), (-1, -1)]
    ]

part1 :: [String] -> Int
part1 input = findall input "XMAS"

checkxmas :: [String] -> Int
checkxmas matrix =
  sum
    [ fromEnum $
        all
          (\((nr, nc), ch) -> isValidPos matrix (nr, nc) ch)
          [ ((r + dr, c + dc), 'M'),
            ((r - dr, c - dc), 'S'),
            ((r + dr, c - dc), 'M'),
            ((r - dr, c + dc), 'S')
          ]
      | (r, row) <- zip [0 ..] matrix,
        (c, char) <- zip [0 ..] row,
        char == 'A',
        (dr, dc) <- [(-1, -1), (-1, 1), (1, -1), (1, 1)]
    ]

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
