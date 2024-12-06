module Main where

import Criterion.Main
import qualified Data.Set as Set
import Utils (readInputLines)

findInit :: [String] -> (Char, (Int, Int))
findInit input =
  head
    [ (char, (r, c))
    | (r, row) <- zip [0 ..] input
    , (c, char) <- zip [0 ..] row
    , char `elem` "^>v<"
    ]

findObs :: [String] -> Set.Set (Int, Int)
findObs input =
  Set.fromList $
    [ (r, c)
    | (r, row) <- zip [0 ..] input
    , (c, char) <- zip [0 ..] row
    , char == '#'
    ]

findNextPos :: Char -> (Int, Int) -> Set.Set (Int, Int) -> Maybe (Char, (Int, Int))
findNextPos ch (r, c) obs =
  case ch of
    '^' -> (\(a, b) -> ('>', (a + 1, b))) <$> Set.lookupLT (r, c) samec
    '>' -> (\(a, b) -> ('v', (a, b - 1))) <$> Set.lookupGT (r, c) samer
    'v' -> (\(a, b) -> ('<', (a - 1, b))) <$> Set.lookupGT (r, c) samec
    '<' -> (\(a, b) -> ('^', (a, b + 1))) <$> Set.lookupLT (r, c) samer
    _ -> Nothing
 where
  samer = Set.filter ((== r) . fst) obs
  samec = Set.filter ((== c) . snd) obs

part1 :: [String] -> Int
part1 input = Set.size $ go d p Set.empty
 where
  obs = findObs input
  (d, p) = findInit input
  height = length input
  width = length $ head input
  go cd cp acc = case findNextPos cd cp obs of
    Just (nd, np) ->
      go
        nd
        np
        ( Set.union
            ( Set.fromList
                [ (r, c)
                | r <- [min (fst cp) (fst np) .. max (fst cp) (fst np)]
                , c <- [min (snd cp) (snd np) .. max (snd cp) (snd np)]
                ]
            )
            acc
        )
    Nothing -> case cd of
      '^' -> Set.union (Set.fromList [(r, snd cp) | r <- [0 .. fst cp]]) acc
      '>' -> Set.union (Set.fromList [(fst cp, c) | c <- [snd cp .. width - 1]]) acc
      'v' -> Set.union (Set.fromList [(r, snd cp) | r <- [fst cp .. height - 1]]) acc
      '<' -> Set.union (Set.fromList [(fst cp, c) | c <- [0 .. snd cp]]) acc
      _ -> acc

findDot :: [String] -> [(Int, Int)]
findDot input =
  [ (r, c)
  | (r, row) <- zip [0 ..] input
  , (c, char) <- zip [0 ..] row
  , char == '.'
  ]

isLoop :: Char -> (Int, Int) -> Set.Set (Int, Int) -> (Int, Int) -> Bool
isLoop d p obs new = go d p Set.empty
 where
  newObs = Set.insert new obs
  go cd cp acc = case findNextPos cd cp newObs of
    Just (nd, np) -> Set.member (nd, np) acc || go nd np (Set.insert (nd, np) acc)
    Nothing -> False

part2 :: [String] -> Int
part2 input = sum $ map (fromEnum . isLoop d p obs) dots
 where
  obs = findObs input
  (d, p) = findInit input
  dots = findDot input

main :: IO ()
main = do
  input <- readInputLines 6
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 1"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
