{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Criterion.Main
import Data.Either
import Data.List (group, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Replace.Megaparsec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (readInputLines)

type Parser = Parsec Void Text

type Coord = (Integer, Integer)
type Robot = (Coord, Coord)

pMachine :: Parser Robot
pMachine = do
  xa <- string "p=" *> L.signed space L.decimal
  ya <- string "," *> L.signed space L.decimal
  xb <- string " v=" *> L.signed space L.decimal
  yb <- string "," *> L.signed space L.decimal
  return ((xa, ya), (xb, yb))

parseInput :: [Text] -> [Robot]
parseInput input = map snd $ rights m
 where
  m = splitCap (match pMachine) $ T.concat input

moves :: Integer -> Coord -> Robot -> Coord
moves sec (w, h) ((x, y), (vx, vy)) = ((x + sec * vx) `mod` w, (y + sec * vy) `mod` h)

safetyFactor :: Coord -> [Coord] -> Int
safetyFactor (w, h) xs = product $ init $ map length $ group . sort $ map quadrant xs
 where
  wm = w `div` 2
  hm = h `div` 2
  quadrant (x, y)
    | x < wm && y < hm = 0
    | x > wm && y < hm = 1
    | x < wm && y > hm = 2
    | x > wm && y > hm = 3
    | otherwise = 4

part1 :: [Text] -> Int
part1 input = safetyFactor (101, 103) $ map (moves 100 (101, 103)) $ parseInput input

displayRobot :: Coord -> [Coord] -> [String]
displayRobot (w, h) r =
  let s = Set.fromList r
   in [[if Set.member (x, y) s then '*' else '.' | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

part2 :: [Text] -> Integer -> [String]
part2 input s = displayRobot (101, 103) $ map (moves s (101, 103)) $ parseInput input

loop :: [Text] -> Integer -> IO ()
loop input n = do
  putStrLn $ "Generated: " ++ show n
  mapM_ putStrLn $ part2 input n
  putStrLn "Press Enter to continue or type 'stop' to quit:"
  i <- getLine
  if i == "stop"
    then putStrLn "Program terminated."
    else loop input (n + 101)

main :: IO ()
main = do
  rawInput <- readInputLines 14
  let input = map T.pack rawInput
  print $ part1 input
  loop input 29
  defaultMain
    [ bgroup
        "Day 14"
        [ bench "part 1" $ nf part1 input
        ]
    ]
