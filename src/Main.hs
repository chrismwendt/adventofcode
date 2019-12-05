{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Loops
import Data.List
import Data.List.Index
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day5

day1 :: IO ()
day1 = do
  input <- readFile "input1.txt"
  let fuel modul = modul `div` 3 - 2
  let allfuel modul = sum $ unfoldr (\fuel -> if fuel <= 0 then Nothing else Just (fuel, fuel `div` 3 - 2)) (fuel modul)
  putStrLn $ "1a: " ++ show (sum $ map (fuel . read) $ lines input)
  putStrLn $ "1b: " ++ show (sum $ map (allfuel . read) $ lines input)

day2 :: IO ()
day2 = do
  input <- readFile "input2.txt"
  let prog = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," input)
  let halt = Map.insert 1 12 . Map.insert 2 2 :: Map.Map Int Int -> Map.Map Int Int
  let set noun verb = Map.insert 1 noun . Map.insert 2 verb :: Map.Map Int Int -> Map.Map Int Int
  let run i p =
        let at = (p Map.!)
         in case at i of
              1 -> run (i + 4) (Map.insert (at (i + 3)) (at (at (i + 1)) + at (at (i + 2))) p)
              2 -> run (i + 4) (Map.insert (at (i + 3)) (at (at (i + 1)) * at (at (i + 2))) p)
              99 -> p
  putStrLn $ "1a: " ++ show ((Map.! 0) $ run 0 $ halt prog)
  let allnvs = [(noun, verb) | noun <- [0 .. 99], verb <- [0 .. 99]]
  let needle (noun, verb) = (== 19690720) $ (Map.! 0) $ run 0 $ set noun verb prog
  putStrLn $ "1b: " ++ show (let (noun, verb) = fromJust $ find needle allnvs in 100 * noun + verb)

day3 :: IO ()
day3 = do
  input <- readFile "input3.txt"
  let dir :: Char -> (Int, Int)
      dir 'U' = (0, 1)
      dir 'D' = (0, -1)
      dir 'L' = (-1, 0)
      dir 'R' = (1, 0)
      rd (c : val) = replicate (read val) (dir c)
      d2w deltas = drop 1 $ scanl (\(x, y) (dx, dy) -> (x + dx, y + dy)) (0, 0) deltas
      (w1 : w2 : _) = map (d2w . concatMap rd . splitOn ",") $ lines input
      lay wire = Map.fromListWith min $ zip wire [1 ..]
      manhattan (x, y) = abs x + abs y
      intersections = Map.intersectionWith (+) (lay w1) (lay w2)
  putStrLn $ "1a: " ++ show (minimum $ map manhattan (Map.keys intersections))
  putStrLn $ "1b: " ++ show (minimum $ Map.elems intersections)

day4 :: IO ()
day4 = do
  let input = "193651-649729"
  let [lower, upper] = map read $ splitOn "-" input :: [Int]
  let adjs xs = zip xs (tail xs)
  let monotonic xs = all (\(a, b) -> a <= b) (adjs xs)
  let meets n =
        and
          [ length (show n) == 6,
            any ((>= 2) . length) (group (show n)),
            monotonic (show n)
          ]
  let meets2 n =
        and
          [ length (show n) == 6,
            any ((== 2) . length) (group (show n)),
            monotonic (show n)
          ]
  putStrLn $ "1a: " ++ show (length $ filter meets [lower .. upper])
  putStrLn $ "1b: " ++ show (length $ filter meets2 [lower .. upper])

day5 :: IO ()
day5 = do
  input <- readFile "input5.txt"
  let prog = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," input)
  let run input i p =
        let at = (p Map.!)
            digit num n = num `div` (10 ^ n) `mod` 10
            get param = case at i `digit` (param - 1 + 2) of
              0 -> at (at (i + param))
              1 -> at (i + param)
         in case at i `mod` 100 of
              1 -> run input (i + 4) (Map.insert (at (i + 3)) (get 1 + get 2) p)
              2 -> run input (i + 4) (Map.insert (at (i + 3)) (get 1 * get 2) p)
              3 -> run (tail input) (i + 2) (Map.insert (at (i + 1)) (head input) p)
              4 -> at (at (i + 1)) : run input (i + 2) p
              5 -> run input (if get 1 /= 0 then get 2 else i + 3) p
              6 -> run input (if get 1 == 0 then get 2 else i + 3) p
              7 -> run input (i + 4) (Map.insert (at (i + 3)) (if get 1 < get 2 then 1 else 0) p)
              8 -> run input (i + 4) (Map.insert (at (i + 3)) (if get 1 == get 2 then 1 else 0) p)
              99 -> []
              other -> error (show (at i))
  putStrLn $ "1a: " ++ show (last $ run [1] 0 prog)
  putStrLn $ "1b: " ++ show (head $ run [5] 0 prog)
