{-# LANGUAGE BlockArguments #-}

module Main where

import Criterion.Main
import Data.Either
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Primitive.PrimArray as P
import Data.SBV
import Data.Void
import Replace.Megaparsec
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Utils (readInputLines)

data Machine = Machine
  { ip :: !Int
  , regUpdates :: !(IntMap Int)
  , regInitial :: !(P.PrimArray Int)
  }
  deriving (Eq, Show, Ord)

indexImage :: Machine -> Int -> Int
indexImage m i
  | i < 3, 0 <= i = P.indexPrimArray a i
  | otherwise = 0
 where
  a = regInitial m

(!) :: Machine -> Int -> Int
m ! i = IntMap.findWithDefault (indexImage m i) i (regUpdates m)

new :: [Int] -> Machine
new initialValues = Machine{ip = 0, regUpdates = IntMap.empty, regInitial = P.primArrayFromList initialValues}

set :: Int -> Int -> Machine -> Machine
set i v m
  | v == o = m{regUpdates = IntMap.delete i (regUpdates m), ip = ip m + 2}
  | otherwise = m{regUpdates = IntMap.insert i v (regUpdates m), ip = ip m + 2}
 where
  o = indexImage m i

jmp :: Int -> Machine -> Machine
jmp i m = m{ip = i}

data Opcode a
  = Adv !a
  | Bxl !a
  | Bst !a
  | Jnz !a
  | Bxc
  | Out !a
  | Bdv !a
  | Cdv !a
  deriving (Eq, Ord, Read, Show)

decode :: Int -> Int -> Maybe (Opcode Int)
decode op oprand =
  case op of
    0 -> Just (Adv oprand)
    1 -> Just (Bxl oprand)
    2 -> Just (Bst oprand)
    3 -> Just (Jnz oprand)
    4 -> Just Bxc
    5 -> Just (Out oprand)
    6 -> Just (Bdv oprand)
    7 -> Just (Cdv oprand)
    _ -> Nothing

data Step
  = Step !Machine
  | StepOut !Int !Machine
  | StepHalt
  | StepFault
  deriving (Show)

step :: Machine -> P.PrimArray Int -> Step
step mach insts
  | ip mach > P.sizeofPrimArray insts - 2 = StepHalt
  | otherwise =
      case decode (P.indexPrimArray insts (ip mach)) (P.indexPrimArray insts (ip mach + 1)) of
        Nothing -> StepFault
        Just op -> opcodeImpl op mach

opcodeImpl :: Opcode Int -> Machine -> Step
opcodeImpl o m =
  case o of
    Adv oprand -> Step (setA (regA `quot` (2 ^ combo oprand)))
    Bxl oprand -> Step (setB (regB `xor` oprand))
    Bst oprand -> Step (setB (combo oprand `mod` 8))
    Jnz oprand -> Step (if regA /= 0 then jmp oprand m else (m{ip = ip m + 2}))
    Bxc -> Step (setB (regB `xor` regC))
    Out oprand -> StepOut (combo oprand `mod` 8) (m{ip = ip m + 2})
    Bdv oprand -> Step (setB (regA `quot` (2 ^ combo oprand)))
    Cdv oprand -> Step (setC (regA `quot` (2 ^ combo oprand)))
 where
  regA = m ! 0
  regB = m ! 1
  regC = m ! 2
  setA n = set 0 n m
  setB n = set 1 n m
  setC n = set 2 n m
  combo od
    | od <= 3, od >= 0 = od
    | od == 4 = regA
    | od == 5 = regB
    | od == 6 = regC
    | otherwise = error "Invalid programs"

data Effect
  = Output !Int Effect
  | Halt
  | Fault
  deriving (Show)

run :: Machine -> P.PrimArray Int -> Effect
run mach insts =
  case step mach insts of
    Step mach' -> run mach' insts
    StepOut out mach' -> Output out (run mach' insts)
    StepHalt -> Halt
    StepFault -> Fault

effectList :: Effect -> [Int]
effectList effect =
  case effect of
    Fault -> error "invalid program"
    Halt -> []
    Output o e -> o : effectList e

type Parser = Parsec Void String

parseInit :: Parser ([Int], [Int])
parseInit = do
  ra <- string "Register A: " *> L.decimal
  rb <- string "Register B: " *> L.decimal
  rc <- string "Register C: " *> L.decimal
  insts <- string "Program: " *> (L.decimal `sepBy` char ',')
  return ([ra, rb, rc], insts)

parseLine :: [String] -> ([Int], [Int])
parseLine input = head $ map snd $ rights $ splitCap (match parseInit) $ concat input

part1 :: [String] -> [Int]
part1 input = effectList $ run m (P.primArrayFromList insts)
 where
  (initReg, insts) = parseLine input
  m = new initReg

direct :: SWord64 -> [Int] -> Symbolic ()
direct a [] = constrain (a .== 0)
direct a (o : os) =
  do
    constrain (a ./= 0)
    b <- pure (a .&. 7)
    b <- pure (b `xor` 6)
    c <- pure (a `sShiftRight` b)
    b <- pure (b `xor` c)
    b <- pure (b `xor` 7)
    constrain ((b .&. 7) .== fromIntegral o)
    a <- pure (a `shiftR` 3)
    direct a os

part2 :: [String] -> IO ()
part2 input = do
  res <- optLexicographic
    do
      a <- free "a" :: Symbolic SWord64
      minimize "smallest" a
      direct a insts
  case getModelValue "a" res of
    Just x -> print (x :: Word64)
    Nothing -> print insts
 where
  (_, insts) = parseLine input

main :: IO ()
main = do
  input <- readInputLines 17
  print $ part1 input
  part2 input
  defaultMain
    [ bgroup
        "Day 17"
        [ bench "part 1" $ nf part1 input
        , bench "part 2" $ whnf part2 input
        ]
    ]
