{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Criterion.Main
import Data.Either
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Replace.Megaparsec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Utils (readInputLines)

type Parser = Parsec Void Text

type Coord = (Integer, Integer)
type Machine = (Coord, Coord, Coord)

pMachine :: Parser Machine
pMachine = do
  xa <- string "Button A: X+" *> decimal
  ya <- string ", Y+" *> decimal
  xb <- string "Button B: X+" *> decimal
  yb <- string ", Y+" *> decimal
  xp <- string "Prize: X=" *> decimal
  yp <- string ", Y=" *> decimal
  return ((xa, ya), (xb, yb), (xp, yp))

parseInput :: [Text] -> [Machine]
parseInput input = map snd $ rights m
 where
  m = splitCap (match pMachine) $ T.concat input

win :: Machine -> Maybe Integer
win ((xa, ya), (xb, yb), (xp, yp)) =
  if ar == 0
    && br == 0
    && x > 0
    && y > 0
    then Just (x * 3 + y)
    else Nothing
 where
  (y, ar) = (ya * xp - xa * yp) `divMod` (ya * xb - xa * yb)
  (x, br) = (xb * yp - yb * xp) `divMod` (ya * xb - xa * yb)

part1 :: [Text] -> Integer
part1 input = sum $ mapMaybe win (parseInput input)

conversion :: Machine -> Machine
conversion (a, b, (xp, yp)) = (a, b, (xp + 10000000000000, yp + 10000000000000))

part2 :: [Text] -> Integer
part2 input = sum $ mapMaybe (win . conversion) (parseInput input)

main :: IO ()
main = do
  rawInput <- readInputLines 13
  let input = map T.pack rawInput
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 1"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
