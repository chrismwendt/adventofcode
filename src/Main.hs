module Main where

import qualified Data.Set as Set

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day1

day1 :: IO ()
day1 = do
  input <- readFile "input1.txt"
  let
    changes = map (read . filter (/= '+')) $ lines input :: [Int]
    frequencies = scanl (+) 0
    firstDupe set (freq : rest)
      | Set.member freq set = freq
      | otherwise = firstDupe (Set.insert freq set) rest

  print $ sum changes
  print $ firstDupe Set.empty (frequencies (cycle changes))
