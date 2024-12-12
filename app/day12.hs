{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonadComprehensions #-}

module Main where

import Criterion.Main
import Data.List (foldl', unfoldr)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Utils (readInputLines)

type Pos = (Int, Int)

parseInput :: [String] -> Map Pos Char
parseInput input = Map.fromList [((r, c), (input !! r) !! c) | r <- [0 .. h - 1], c <- [0 .. w - 1]]
 where
  h = length input
  w = length (head input)

dfs :: (Ord t) => (a -> t) -> (a -> [a]) -> [a] -> [a]
dfs rep next = loop Set.empty
 where
  loop seen a = case a of
    [] -> []
    x : xs
      | Set.member r seen -> loop seen xs
      | otherwise -> x : loop seen1 (next x ++ xs)
     where
      r = rep x
      seen1 = Set.insert r seen

cardinal :: Pos -> [Pos]
cardinal (r, c) = [(r + 1, c), (r - 1, c), (r, c + 1), (r, c - 1)]

regions :: Map Pos Char -> [Set Pos]
regions =
  unfoldr \input ->
    [ (region, Map.withoutKeys input region)
    | (start, label) <- Map.lookupMin input
    , let region = Set.fromList (dfs id step [start])
          step i = [j | j <- cardinal i, Map.lookup j input == Just label]
    ]

perimeter :: Set Pos -> Int
perimeter xs = length [() | x <- Set.toList xs, y <- cardinal x, y `Set.notMember` xs]

part1 :: [String] -> Int
part1 input =
  let rs = regions (parseInput input)
   in sum $ map (\x -> perimeter x * length x) rs

sides :: Set Pos -> Int
sides xs =
  count (corner left above) xs
    + count (corner above right) xs
    + count (corner below left) xs
    + count (corner right below) xs
 where
  corner d1 d2 x = open d1 && (open d2 || not (open (d1 . d2)))
   where
    open d = d x `Set.notMember` xs
  count f = foldl' (\acc x -> if f x then acc + 1 else acc) 0
  left (x, y) = (x - 1, y)
  right (x, y) = (x + 1, y)
  above (x, y) = (x, y + 1)
  below (x, y) = (x, y - 1)

part2 :: [String] -> Int
part2 input =
  let rs = regions (parseInput input)
   in sum $ map (\x -> sides x * length x) rs

main :: IO ()
main = do
  input <- readInputLines 12
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 12"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
