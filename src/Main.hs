{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.List.Index
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Control.Monad.Loops
import Linear.V2
import Data.Array
import Safe
import Data.Ord
import Data.Tuple.Extra
import Data.Foldable

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day4

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

    claimParser :: Parsec Void String (Int, Int, Int, Int, Int)
    claimParser = do
      id <- char '#' *> int <* string " @ "
      (left, top) <- (,) <$> decimal <* char ',' <*> decimal
      string ": "
      (width, height) <- (,) <$> decimal <* char 'x' <*> decimal
      char '\n'
      return (id, left, top, width, height)

    claimToCells (id, left, top, width, height) =
      [ (V2 (left + a) (top + b), [id])
      | a <- [0 .. width - 1]
      , b <- [0 .. height - 1]
      ]

    size = (V2 0 0, V2 1000 1000)
    stack claims = accumArray (++) [] size $ (concatMap claimToCells) claims
    measureOverlap claims = sum $ fmap (\ids -> if length ids >= 2 then 1 else 0) $ stack claims
    findNonOverlapping claims =
      headMay
      $ Set.toList . foldl' (flip Set.delete) (Set.fromList (map (\(id, _, _, _, _) -> id) claims))
      $ concat . filter (\ids -> length ids >= 2)
      $ toList (stack claims)

  putStrLn $ "3a: " ++ maybe "BUG" (show . measureOverlap) (parseMaybe (many claimParser) input)
  putStrLn $ "3b: " ++ maybe "BUG" show (parseMaybe (many claimParser) input >>= findNonOverlapping)

data Time = Time Int Int Int Int Int deriving (Eq, Ord, Show)
data Log = Begin Int | Sleep | Wake deriving (Eq, Ord, Show)
type Minute = Int

day4 :: IO ()
day4 = do
  input <- readFile "input4.txt"

  let
    int = fromIntegral <$> decimal
    logParser :: Parsec Void String (Time, Log)
    logParser = do
      char '['
      y <- int <* char '-'
      mo <- int <* char '-'
      d <- int <* char ' '
      h <- int <* char ':'
      mi <- int
      char ']'
      char ' '
      l <- choice
        [ Begin <$> (string "Guard #" *> int <* string " begins shift")
        , Sleep <$ string "falls asleep"
        , Wake <$ string "wakes up"
        ]
      char '\n'
      return $ (Time y mo d h mi, l)

    minute (Time _ _ _ _ m) = m
    frequencies = Map.fromListWith (+) . map (, 1)
    range a b = [a .. b - 1]
    keyOfMax = fmap fst . maximumByMay (comparing snd) . Map.toList
    minuteMode xs = keyOfMax $ frequencies $ concatMap (uncurry range) xs
    totalSleep = sum . map (\(a, b) -> b - a)
    groupByKey = Map.fromListWith (++) . map (\(a, b) -> (a, [b]))
    minuteOnly = map (\(t, g) -> (minute t, g))
    guardNaps :: [(Minute, Log)] -> [(Int, (Minute, Minute))]
    guardNaps xs =
      let
        go _ [] = []
        go _ ((t, Begin g) : rest) = go (Just (t, g)) rest
        go Nothing _ = [] -- error: this means the log did not start with Begin
        go (Just (_, g)) ((t, Sleep) : rest) = go (Just (t, g)) rest
        go (Just (t, g)) ((t2, Wake) : rest) = (g, (t, t2)) : go (Just (t2, g)) rest
      in
        go Nothing xs
    partA logs = do
      (guard, naps) <- maximumByMay (comparing $ totalSleep . snd)
        $ Map.toList $ groupByKey $ guardNaps $ minuteOnly $ sort logs
      minute <- minuteMode naps
      return $ guard * minute
    partB logs = do
      maxGuardDurationByMinute <- mapM (\(a, b) -> (a, ) <$> maximumByMay (comparing snd) b)
        $ Map.toList $ fmap Map.toList $ fmap frequencies $ groupByKey
        $ concatMap ((\(g, ts) -> map (, g) ts) . second (uncurry range))
        $ guardNaps $ minuteOnly $ sort logs
      (minute, (guard, _)) <- maximumByMay (comparing (snd . snd)) maxGuardDurationByMinute
      return $ guard * minute

  putStrLn $ "4a: " ++ maybe "BUG" show (parseMaybe (many logParser) input >>= partA)
  putStrLn $ "4a: " ++ maybe "BUG" show (parseMaybe (many logParser) input >>= partB)
