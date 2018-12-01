module Main where

import qualified Data.Set as Set
import Data.Maybe

main :: IO ()
main = day1part2

ghci :: IO ()
ghci = main

day1part1 :: IO ()
day1part1 = do
  input <- readFile "input1.txt"
  print $ sum $ map (read . filter (/= '+')) $ lines input

day1part2 :: IO ()
day1part2 = do
  input <- readFile "input1.txt"
  let
    frequencies = scanl (+) 0 $ map (read . filter (/= '+')) $ cycle $ lines input :: [Int]
    dupes set (freq : rest) = (if Set.member freq set then [freq] else []) ++ dupes (Set.insert freq set) rest
  print $ head $ dupes Set.empty frequencies
