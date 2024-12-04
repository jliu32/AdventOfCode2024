module Utils (readInputLines, readInput) where

import System.IO (readFile)

-- Read input file as lines
readInputLines :: Int -> IO [String]
readInputLines day = do
    let filePath = "data/day" ++ show day ++ "_input.txt"
    content <- readFile filePath
    return (lines content)

-- Read input as a raw string
readInput :: Int -> IO String
readInput day = do
    let filePath = "data/day" ++ show day ++ "_input.txt"
    readFile filePath
