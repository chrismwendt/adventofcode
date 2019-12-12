{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad.Loops
import Data.Char
import Data.Function
import Data.List
import Data.List.Extra (maximumOn)
import Data.List.Index
import Data.List.Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Ord
import Data.Ratio
import qualified Data.Set as Set
import Text.Printf

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day10

day1 :: IO ()
day1 = do
  input <- readFile "input1.txt"
  let fuel modul = modul `div` 3 - 2
  let allfuel modul = sum $ unfoldr (\fuel -> if fuel <= 0 then Nothing else Just (fuel, fuel `div` 3 - 2)) (fuel modul)
  putStrLn $ "1a: " ++ show (sum $ map (fuel . read) $ lines input)
  putStrLn $ "1b: " ++ show (sum $ map (allfuel . read) $ lines input)

type State = (Int, Int, Map.Map Int Int)

data Step = Halt | Input (Int -> State) | Output (Int, State)

step :: State -> Step
step (base, pc, mem) =
  let val i = fromMaybe 0 $ mem Map.!? i
      digit num n = num `div` (10 ^ n) `mod` 10
      l param = case val pc `digit` (param - 1 + 2) of
        0 -> val (pc + param) -- position
        1 -> pc + param -- immediate
        2 -> base + val (pc + param) -- relative
      r = val . l
   in case val pc `mod` 100 of
        1 -> step (base, pc + 4, Map.insert (l 3) (r 1 + r 2) mem) -- add
        2 -> step (base, pc + 4, Map.insert (l 3) (r 1 * r 2) mem) -- mul
        3 -> Input (\i -> (base, pc + 2, Map.insert (l 1) i mem)) -- input
        4 -> Output (r 1, (base, pc + 2, mem)) -- output
        5 -> step (base, (if r 1 /= 0 then r 2 else pc + 3), mem) -- jnz
        6 -> step (base, (if r 1 == 0 then r 2 else pc + 3), mem) -- jz
        7 -> step (base, pc + 4, Map.insert (l 3) (if r 1 < r 2 then 1 else 0) mem) -- lt
        8 -> step (base, pc + 4, Map.insert (l 3) (if r 1 == r 2 then 1 else 0) mem) -- eq
        9 -> step (base + r 1, pc + 2, mem) -- adjust base
        99 -> Halt
        other -> error ("Invalid opcode " ++ show (val pc))

run input0 mem0 =
  let recur input state = case step state of
        Halt -> []
        Input feed -> recur (tail input) $ feed (head input)
        Output (o, state') -> o : recur input state'
   in recur input0 (0, 0, mem0)

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
  putStrLn $ "1a: " ++ show (last $ run [1] prog)
  putStrLn $ "1b: " ++ show (head $ run [5] prog)

dup x = (x, x)

unfold :: (a -> Maybe a) -> a -> [a]
unfold f z = unfoldr (\b -> dup <$> f b) z

unfoldz :: (a -> Maybe a) -> a -> [a]
unfoldz f z = z : unfoldr (\b -> dup <$> f b) z

day6 :: IO ()
day6 = do
  input <- readFile "input6.txt"
  let dagpairs = map (splitOn ")") $ lines input
  let m1 = Map.fromList $ map (\(a : b : _) -> (b, a)) dagpairs
  let orbits p = unfoldr (\b -> let x = m1 Map.!? b in dup <$> x) p
  let path label = Map.fromList $ unfold (\(cl, n) -> (,n + 1) <$> (m1 Map.!? cl)) (label, 0)
  putStrLn $ "1a: " ++ show (sum $ map (length . orbits) $ Map.keys m1)
  putStrLn $ "1b: " ++ show (minimum $ Map.elems $ Map.intersectionWith (+) (path (m1 Map.! "YOU")) (path (m1 Map.! "SAN")))

day7 :: IO ()
day7 = do
  input <- readFile "input7.txt"
  let prog = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," input)
  let try phases = (foldl (\prev phase -> run (phase : prev) prog) [0] phases)
  let tryfeed phases = let signal = foldl (\prev phase -> run (phase : prev) prog) (0 : signal) phases in signal
  putStrLn $ "1a: " ++ show (maximum $ map (last . try) (permutations [0 .. 4]))
  putStrLn $ "1b: " ++ show (maximum $ map (last . tryfeed) (permutations [5 .. 9]))

day8 :: IO ()
day8 = do
  input <- readFile "input8.txt"
  let count f = length . filter f
  let b = chunksOf (25 * 6) $ map (digitToInt :: Char -> Int) input
  let a = minimumBy (comparing (count (== 0))) b
  putStrLn $ "1a: " ++ show (count (== 1) a * count (== 2) a)
  let view '0' = ' '
      view '1' = '#'
      view '2' = '.'
  let merge '.' a = a
      merge a _ = a
  putStrLn $ "1b: " ++ "\n" ++ unlines (foldl1' (zipWith (zipWith merge)) $ chunksOf 6 $ chunksOf 25 $ map view $ input)

day9 :: IO ()
day9 = do
  in0 <- readFile "input9.txt"
  let mem0 = Map.fromList . zip [0 ..] $ map read $ splitOn "," in0
  putStrLn $ "1a: " ++ show (head $ run [1] mem0)
  putStrLn $ "1b: " ++ show (head $ run [2] mem0)

v2minus :: (Int, Int) -> (Int, Int) -> (Int, Int)
v2minus (ax, ay) (bx, by) = (ax - bx, ay - by)

v2plus :: (Int, Int) -> (Int, Int) -> (Int, Int)
v2plus (ax, ay) (bx, by) = (ax + bx, ay + by)

man :: (Int, Int) -> Int
man (x, y) = abs x + abs y

uniqueOn :: Ord b => (a -> b) -> [a] -> [a]
uniqueOn proj as = Map.elems $ Map.fromList $ map (\a -> (proj a, a)) as

-- sorts by proj
groupOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupOn proj as = Map.elems $ Map.fromListWith (++) $ map (\a -> (proj a, [a])) as

day10 :: IO ()
day10 = do
  in0 <- readFile "input10.txt"
  let asteroids = catMaybes $ concatMap (\(y, row :: String) -> zipWith (\x c -> if c == '.' then Nothing else Just (x, y)) [0 ..] row) $ zip [0 ..] (lines in0)
      -- returns a value that can be used to sort vectors by angle
      angl (0, 0) = error "angl on (0, 0)"
      angl (x, y) =
        let slope = (if abs x < abs y then negate (x % y) else y % x) / 2
            quadrant =
              foldl'
                (\acc f -> 2 * acc + if f then 1 else 0)
                0
                [ x < 0 || (x == 0 && y > 0),
                  y == 0 || (x > 0 && y > 0) || (x < 0 && y < 0),
                  slope < 0
                ]
         in quadrant + slope
      bestCoords = maximumOn uniqueAngles asteroids
      others = filter (/= bestCoords) asteroids
      uniqueAngles coords = length $ uniqueOn angl $ map (`v2minus` coords) $ filter (/= coords) asteroids
      vaporize = map (`v2plus` bestCoords) $ concat $ transpose $ map (sortBy (comparing man)) $ groupOn angl $ map (`v2minus` bestCoords) others
  putStrLn $ "1a: " ++ show (uniqueAngles bestCoords)
  putStrLn $ "1b: " ++ show (let (x, y) = vaporize !! (200 - 1) in 100 * x + y)

day11 :: IO ()
day11 = do
  in0 <- readFile "input11.txt"
  let prog = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," in0)
  let hull color0 =
        let recur :: State -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) () -> (Int, Int) -> Int -> (Map.Map (Int, Int) Int, Map.Map (Int, Int) ())
            recur state h ptd pos dir = case step state of
              Halt -> (h, ptd)
              Input feed -> recur (feed (fromMaybe 0 (h Map.!? pos))) h ptd pos dir
              Output (color, state') -> case step state' of
                Output (turn, state'') ->
                  let dir' = (if turn == 0 then (+ negate 1) else (+ 1)) dir `mod` 4
                      pos' = case dir' of
                        0 -> pos `v2plus` (0, -1)
                        1 -> pos `v2plus` (1, 0)
                        2 -> pos `v2plus` (0, 1)
                        3 -> pos `v2plus` (-1, 0)
                   in recur state'' (Map.insert pos color h) (Map.insert pos () ptd) pos' dir'
                Halt -> error "bad halt"
                Input _ -> error "bad input"
         in recur (0, 0, prog) (Map.singleton (0, 0) color0) Map.empty (0, 0) 0
  putStrLn $ "1a: " ++ show (Map.size $ snd (hull 0))
  let pretty hull =
        ("\n" ++) $
          unlines
            [ [ if fromMaybe 0 (hull Map.!? (x, y)) == 1 then '#' else '.'
                | x <- [0 .. maximum (map fst $ Map.keys hull)]
              ]
              | y <- [0 .. maximum (map snd $ Map.keys hull)]
            ]
  putStrLn $ "1b: " ++ pretty (fst $ hull 1)
