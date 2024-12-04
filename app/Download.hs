{-# LANGUAGE OverloadedStrings #-}

module Main where

-- For requestHeaders
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import Network.HTTP.Client (Request (..))
import Network.HTTP.Conduit (httpLbs, newManager, parseRequest, responseBody, tlsManagerSettings)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)

-- Function to download the input
downloadInput :: Int -> IO ()
downloadInput day = do
  let url = "https://adventofcode.com/2024/day/" ++ show day ++ "/input"
  cookie <- BS.readFile "session.txt" -- Read session cookie from file
  let headers = [("Cookie", "session=" <> cookie)]
  request <- parseRequest url
  let request' = request {requestHeaders = headers}
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager
  let outputDir = "data/"
  createDirectoryIfMissing True outputDir
  let outputFile = outputDir ++ "day" ++ show day ++ "_input.txt"
  TIO.writeFile outputFile (T.strip $ T.decodeUtf8 $ BS.toStrict $ responseBody response)
  putStrLn $ "Input for Day " ++ show day ++ " downloaded to " ++ outputFile

-- Main function to handle CLI arguments
main :: IO ()
main = do
  args <- getArgs
  case args of
    [day] -> downloadInput (read day)
    _ -> putStrLn "Usage: cabal run advent-of-code -- <day>"
