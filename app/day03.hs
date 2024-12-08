module Main where

import Criterion.Main
import Data.Bifunctor (second)
import Data.Either
import Data.Void
import Replace.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Utils (readInputLines)

mulparser :: Parsec Void String (Int, Int)
mulparser = do
  x1 <- string "mul(" >> decimal
  x2 <- char ',' >> decimal
  _ <- char ')'
  return (x1, x2)

part1 :: [String] -> Int
part1 input = sum $ map (uncurry (*)) commands
 where
  commands = rights $ second snd <$> splitCap (match mulparser) (concat input)

enparser :: Parsec Void String Bool
enparser = do
  d <- string "do()" <|> string "don't()"
  if d == "do()"
    then do
      return True
    else
      return False

disgard :: String -> String
disgard input = concat . lefts $ head xs : [xs !! (r + 1) | r <- [1, 3 .. length xs - 1], fromRight False (xs !! r)]
 where
  xs = second snd <$> splitCap (match enparser) input

part2 :: [String] -> Int
part2 input = sum $ map (uncurry (*)) commands
 where
  mem = disgard $ unlines input
  commands = rights $ second snd <$> splitCap (match mulparser) mem

main :: IO ()
main = do
  input <- readInputLines 3
  print $ part1 input
  print $ part2 input
  defaultMain
    [ bgroup
        "Day 3"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ nf part2 input
        ]
    ]
