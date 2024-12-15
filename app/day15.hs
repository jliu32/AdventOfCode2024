module Main where

import Criterion.Main
import Data.List.Split (chunksOf, splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Utils (readInputLines)

newtype Warehouse = Warehouse {cells :: Map Pos Char}
  deriving (Show)

type Pos = (Int, Int)

left, right, above, below :: Pos -> Pos
left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)
above (r, c) = (r - 1, c)
below (r, c) = (r + 1, c)

toMove :: Char -> Pos -> Pos
toMove c
  | c == '<' = left
  | c == '>' = right
  | c == '^' = above
  | c == 'v' = below
  | otherwise = id

parseInput :: [String] -> (Pos, Warehouse, [Pos -> Pos])
parseInput input = (initPos, Warehouse warehouse, moves)
 where
  [warehouseMap, robotMoves] = splitOn [""] input
  warehouse =
    M.fromList
      [ ((r, c), ch)
      | (r, row) <- zip [0 ..] warehouseMap
      , (c, ch) <- zip [0 ..] row
      , ch /= '.'
      ]
  initPos = head $ M.keys (M.filter (== '@') warehouse)
  moves = map toMove (unlines robotMoves)

-- part1 :: [String] -> Int
part1 input = score $ cells finalBoxes
 where
  (initPos, warehouse, moves) = parseInput input
  (finalBoxes, _) = foldl move2 (warehouse, initPos) moves
  score m = sum [100 * y + x | ((y, x), c) <- M.assocs m, c == 'O']

convert :: Char -> String
convert c
  | c == '#' = "##"
  | c == 'O' = "[]"
  | c == '.' = ".."
  | c == '@' = "@."
  | c == '\n' = "\n"
  | otherwise = ""

convertInput :: [String] -> (Pos, Warehouse, [Pos -> Pos])
convertInput input = (initPos, Warehouse warehouse, moves)
 where
  [warehouseMap, robotMoves] = splitOn [""] input
  newWarehouseMap = map (concatMap convert) warehouseMap
  warehouse =
    M.fromList
      [ ((r, c), ch)
      | (r, row) <- zip [0 ..] newWarehouseMap
      , (c, ch) <- zip [0 ..] row
      , ch /= '.'
      ]
  initPos = head $ M.keys (M.filter (== '@') warehouse)
  moves = map toMove (unlines robotMoves)

move2 :: (Warehouse, Pos) -> (Pos -> Pos) -> (Warehouse, Pos)
move2 (warehouse, r) d =
  case go M.empty [r] of
    Nothing -> (warehouse, r)
    Just region -> (warehouse', d r)
     where
      warehouse' =
        Warehouse $
          M.union
            (M.mapKeysMonotonic d region)
            (M.difference (cells warehouse) region)
 where
  vertical = fst (d (0, 0)) /= 0

  go seen [] = Just seen
  go seen (x : xs)
    | M.notMember x seen
    , Just c <- M.lookup x (cells warehouse) =
        if c == '#'
          then Nothing
          else
            let next = case c of
                  '[' -> [right x | vertical] ++ [d x]
                  ']' -> [left x | vertical] ++ [d x]
                  'O' -> [d x]
                  '@' -> [d x]
                  _ -> []
             in go (M.insert x c seen) (next ++ xs)
    | otherwise = go seen xs

part2 :: [String] -> Int
part2 input = score $ cells finalBoxes
 where
  (initPos, warehouse, moves) = convertInput input
  (finalBoxes, _) = foldl move2 (warehouse, initPos) moves
  score m = sum [100 * y + x | ((y, x), c) <- M.assocs m, c == '[']

main :: IO ()
main = do
  input <- readInputLines 15
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 15"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
