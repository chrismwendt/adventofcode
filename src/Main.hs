{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List.Index
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Control.Monad.Loops
import Linear.V2
import Data.Array

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day3

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

day3 :: IO ()
day3 = do
  input <- readFile "input3.txt"

  let
    int = fromIntegral <$> decimal

    lineParser :: Parsec Void String (Int, Int, Int, Int)
    lineParser = do
      char '#' >> int >> string " @ "
      (left, top) <- (,) <$> decimal <* char ',' <*> decimal
      string ": "
      (width, height) <- (,) <$> decimal <* char 'x' <*> decimal
      char '\n'
      return (left, top, width, height)

    claimToCells (left, top, width, height) =
      [ V2 (left + a) (top + b)
      | a <- [0 .. width - 1]
      , b <- [0 .. height - 1]
      ]

    size = (V2 0 0, V2 1000 1000)
    stack claims = accumArray (+) 0 size $ map (, 1) $ (concatMap claimToCells) claims
    overlap claims = sum $ fmap (\depth -> if depth >= 2 then 1 else 0) $ stack claims

  putStrLn $ "3a: " ++ maybe "BUG" (show . overlap) (parseMaybe (many lineParser) input)
