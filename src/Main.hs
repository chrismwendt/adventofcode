{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import Data.List.Index
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Void
import Control.Monad
import Control.Monad.Loops
import Linear.V2
import Data.Array
import Safe (minimumMay, maximumMay, maximumByMay, maximumDef, headMay)
import Data.Ord
import Data.Tuple.Extra
import Control.Monad.Trans.State.Lazy
import Lens.Micro.Platform
import Data.Char
import Data.Function
import Control.Error.Util
import Data.Foldable

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day6

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
    range a b = [a .. b - 1]
    maxInMap = maximumByMay (comparing snd) . Map.toList
    maxInMapBy f = maximumByMay (comparing (f . snd)) . Map.toList
    maximumDef' f = maximumDef f . toList

    flattenMaps :: (Ord k1, Ord k2) => Map.Map k1 (Map.Map k2 a) -> Map.Map (k1, k2) a
    flattenMaps =
      Map.fromList
      . map (\(k1, (k2, v)) -> ((k1, k2), v))
      . concatMap (\(k, kvs) -> map (k, ) kvs)
      . Map.toList . fmap Map.toList

    f (_, _, m) (_, Begin guard) = (guard, 0, m)
    f (guard, _, m) (start, Sleep) = (guard, start, m)
    f (guard, start, m) (stop, Wake) = (guard, 0, foldl' (flip add) m (range start stop))
      where
      add minute = at guard . non Map.empty . at minute . non 0 %~ (+ 1)

    sleepByMinuteByGuard logs = thd3 $ foldl' f (0, 0, Map.empty) $ map (first minute) $ sort logs

    partA logs = do
      (g, sleepByMinute) <- maxInMapBy sum $ sleepByMinuteByGuard logs
      (m, _) <- maxInMap sleepByMinute
      return $ g * m

    partB logs = do
      ((g, m), _) <- maxInMap $ flattenMaps $ sleepByMinuteByGuard logs
      return $ g * m

  putStrLn $ "4a: " ++ maybe "BUG" show (parseMaybe (many logParser) input >>= partA)
  putStrLn $ "4b: " ++ maybe "BUG" show (parseMaybe (many logParser) input >>= partB)

data Zipper a = Zipper [a] [a] deriving (Show)

zNew :: [a] -> Zipper a
zNew as = Zipper [] as

zToList :: Zipper a -> [a]
zToList (Zipper ls rs) = reverse ls ++ rs

zRight :: Zipper a -> Maybe (Zipper a)
zRight (Zipper _ []) = Nothing
zRight (Zipper ls (a : rest)) = Just $ Zipper (a : ls) rest

unique :: Ord a => [a] -> [a]
unique = Set.toList . Set.fromList

day5 :: IO ()
day5 = do
  input <- readFile "input5.txt"

  let
    lastIteration f a = maybe a (lastIteration f) (f a)

    react :: Zipper Char -> Maybe (Zipper Char)
    react (Zipper [] _) = Nothing
    react (Zipper _ []) = Nothing
    react (Zipper (l : ls) (r : rs)) = if ((/=) `on` isUpper) l r && ((==) `on` toUpper) l r
      then Just $ Zipper ls rs
      else Nothing

    fullReact :: [Char] -> [Char]
    fullReact i = zToList $ lastIteration (\z -> lastIteration react <$> zRight z) $ zNew i

    partA = length $ fullReact input
    partB = minimumMay
      $ map (\part -> length $ fullReact $ filter ((/= part) . toUpper) input)
      $ unique $ map toUpper input

  putStrLn $ "5a: " ++ show partA
  putStrLn $ "5b: " ++ maybe "BUG" show partB


day6 :: IO ()
day6 = do
  input <- readFile "input6.sample.txt"

  let
    int = fromIntegral <$> decimal

    coordParser :: Parsec Void String (Int, Int)
    coordParser = pure (,) <*> int <* string ", " <*> int <* char '\n'

    partA _ = return ()
    partB _ = return ()

  putStrLn $ "6a: " ++ maybe "BUG" show (parseMaybe (many coordParser) input >>= partA)
  putStrLn $ "6b: " ++ maybe "BUG" show (parseMaybe (many coordParser) input >>= partB)
