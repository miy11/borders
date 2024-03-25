module Main where

import Tabulate

filePath :: FilePath
filePath = "resources/BordersRaw.txt"

main :: IO ()
main = do
  contents <- readFile filePath
  let result = borders (lines contents)
  print result