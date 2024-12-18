module Main where

import Criterion.Main
import Data.Array
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (readInputLines)

type Parser = Parsec Void String

type Coord = (Int, Int)
type Dir = Coord
type State = (Coord, Dir)
type Maze = Array Coord Char

numberPair :: Parser (Int, Int)
numberPair = do
  x <- L.decimal
  _ <- char ','
  y <- L.decimal
  return (x, y)

-- Parser for multiple number pairs separated by newlines
numberPairList :: Parser [(Int, Int)]
numberPairList = numberPair `sepEndBy` newline

-- Top-level function to parse input
parseNumberPairs :: String -> Either (ParseErrorBundle String Void) [(Int, Int)]
parseNumberPairs = runParser (numberPairList <* eof) ""

parseInput :: [String] -> [(Int, Int)]
parseInput input = case parseNumberPairs (unlines input) of
  Left err -> error (show err)
  Right pairs -> pairs

new :: [(Int, Int)] -> Maze
new corrupted = array ((0, 0), (70, 70)) [((r, c), if (r, c) `elem` corrupted then '#' else '.') | r <- [0 .. 70], c <- [0 .. 70]]

directions :: [Dir]
directions = [(-1, 0), (1, 0), (0, -1), (0, 1)]

turnLeft, turnRight :: Dir -> Dir
turnLeft (dx, dy) = (-dy, dx)
turnRight (dx, dy) = (dy, -dx)

move :: Coord -> Dir -> Coord
move (x, y) (dx, dy) = (x + dx, y + dy)

isValidMove :: Maze -> Coord -> Bool
isValidMove maze pos =
  let ((minX, minY), (maxX, maxY)) = bounds maze
   in x >= minX && x <= maxX && y >= minY && y <= maxY && (maze ! pos == '.')
 where
  (x, y) = pos

search :: Maze -> Set Coord -> IntMap (Map Coord (Set Coord)) -> (Int, Int)
search maze seen queue =
  case IM.minViewWithKey queue of
    Nothing -> (-1, -1)
    Just ((cost, current), restQueue)
      | not (null ends) -> (cost, length (mconcat ends))
      | otherwise -> search maze seen' newQueue
     where
      ends = [b | (e, b) <- M.assocs newStates, e == (70, 70)]
      seen' =
        S.union seen $
          S.fromList (M.keys newStates)
      newStates = M.withoutKeys current seen
      newQueue =
        IM.unionWith merge restQueue $
          IM.fromListWith
            merge
            [ next
            | (p, path) <- M.assocs newStates
            , next <-
                [(cost + 1, M.singleton (move p d) (S.insert (move p d) path)) | d <- directions, isValidMove maze (move p d)]
            ]
      merge = M.unionWith S.union

part1 :: [String] -> Int
part1 input = fst $ search m S.empty (IM.singleton 0 (M.singleton (0, 0) (S.singleton (0, 0))))
 where
  c = parseInput input
  m = new (take 1024 c)

part2 :: [String] -> (Int, Int)
part2 input = c !! (bs 0 (length c) - 1)
 where
  c = parseInput input
  go n =
    fst $ search (new (take n c)) S.empty (IM.singleton 0 (M.singleton (0, 0) (S.singleton (0, 0))))
  bs l h
    | l > h = -1
    | otherwise =
        let m = (l + h) `div` 2
            mv = go m
         in if mv < 0
              then
                if go (m - 1) >= 0
                  then m
                  else bs l (m - 1)
              else bs (m + 1) h

main :: IO ()
main = do
  input <- readInputLines 18
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 18"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
