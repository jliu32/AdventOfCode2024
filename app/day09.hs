module Main where

import Criterion.Main
import Data.List (partition, sort)
import Utils (readInputLines)

parseLine :: String -> ([[(Int, Int)]], Int, Int)
parseLine line = foldl f ([], 0, 0) $ zip [0 ..] line
 where
  f (acc, fidx, idx) (i, ch)
    | even i =
        if ch == '0'
          then (acc, fidx, idx)
          else (acc ++ [zip (replicate s fidx) [idx .. idx + s]], fidx + 1, idx + s)
    | otherwise = (acc ++ [zip (replicate s (-1)) [idx .. idx + s]], fidx, idx + s)
   where
    s = read [ch]

move :: [Int] -> [(Int, Int)]
move xs = moved ++ take (files - freeSpaceFront) (dropWhile (\(x, _) -> x < 0) ys)
 where
  ys = sort (zip xs [0 ..])
  freeSpace = length $ takeWhile (\(x, _) -> x < 0) ys
  files = length xs - freeSpace
  freeSpaceFront = length $ filter (< 0) $ take files xs
  p = take freeSpaceFront $ zip ys (reverse ys)
  moved = map swap p
  swap ((_, i1), (a2, _)) = (a2, i1)

part1 :: [String] -> Int
part1 input = sum $ map (uncurry (*)) $ move $ map fst (concat disk)
 where
  (disk, _, _) = parseLine $ head input

parseDisk :: String -> [Int]
parseDisk line = foldl f [] (zip [0 ..] line)
 where
  f acc (i, ch)
    | even i = acc ++ [-1]
    | otherwise = acc ++ [read [ch]]

moveChunk :: [[(Int, Int)]] -> [Int] -> [(Int, Int)]
moveChunk xs ds = snd $ foldr moves (u, []) v
 where
  (u, v) = partition (\x -> snd x >= 0) $ zip xs ds
  swap ((_, i1), (a2, _)) = (a2, i1)
  moves x@(fs, _) (mem, mv) =
    let need = length fs
        (f, b) = span (\(_, s) -> s < need) mem
     in if null b || null fs
          then (mem, mv ++ fs)
          else
            let (free, size) = head b
                (_, l) = head free
                (_, h) = head fs
                ns = size - need
                moved = zipWith (curry swap) (take need free) fs
                movedMem = zipWith (curry swap) fs (take need free)
             in if l < h
                  then
                    (f ++ [(drop need free, ns)] ++ tail b ++ [(movedMem, need)], mv ++ moved)
                  else
                    (mem, mv ++ fs)

part2 :: [String] -> Int
part2 input = sum $ map (uncurry (*)) $ moveChunk xs ds
 where
  (xs, _, _) = parseLine (head input)
  ds = parseDisk (head input)

main :: IO ()
main = do
  input <- readInputLines 9
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 09"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
