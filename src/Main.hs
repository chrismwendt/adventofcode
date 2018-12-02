{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Index
import Control.Monad.Loops

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day1
  day2

firstDupe :: Ord a => [a] -> Maybe a
firstDupe = go Set.empty
  where
  go _ [] = Nothing
  go set (a : rest)
    | Set.member a set = Just a
    | otherwise = go (Set.insert a set) rest

day1 :: IO ()
day1 = do
  input <- readFile "input1.txt"

  let
    changes = map (read . filter (/= '+')) $ lines input :: [Int]
    frequencies = scanl (+) 0

  putStrLn $ "1a: " ++ show (sum changes)
  putStrLn $ "1b: " ++ maybe "BUG" show (firstDupe (frequencies (cycle changes)))

day2 :: IO ()
day2 = do
  input <- readFile "input2.txt"

  let
    count p = length . filter p
    frequencies = map (Map.elems . Map.fromListWith (+) . map (, 1)) $ lines input
    checksum = count (2 `elem`) frequencies * count (3 `elem`) frequencies

  putStrLn $ "2a: " ++ show checksum

  let
    dropEachLetter s = imap (\i _ -> (i, deleteAt i s)) s

  putStrLn $ "2b: " ++ maybe "BUG" snd (firstDupe $ concat $ map dropEachLetter $ lines input)
