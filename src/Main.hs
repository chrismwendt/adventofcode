{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Conduit
import Control.Monad
import qualified Control.Monad.Combinators as P
import Control.Monad.Loops
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Char
import qualified Data.Conduit.List as CL
import Data.Foldable
import Data.Function
import Data.IORef
import Data.List
import Data.List.Extra (maximumOn)
import Data.List.Index
import Data.List.Split
import qualified Data.Map as Map
import Data.Map ((!), (!?), Map)
import Data.Maybe
import Data.Ord
import Data.Ratio
import Data.STRef
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set (Set)
import Debug.Trace
import System.IO
import System.Random
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Text.Printf

ghci :: IO ()
ghci = main

main :: IO ()
main = do
  day15

day1 :: IO ()
day1 = do
  input <- readFile "input1.txt"
  let fuel modul = modul `div` 3 - 2
  let allfuel modul = sum $ unfoldr (\fuel -> if fuel <= 0 then Nothing else Just (fuel, fuel `div` 3 - 2)) (fuel modul)
  putStrLn $ "1a: " ++ show (sum $ map (fuel . read) $ lines input)
  putStrLn $ "1b: " ++ show (sum $ map (allfuel . read) $ lines input)

type VMState = (Int, Int, Map.Map Int Int)

data SingleStep = Halt | Input (Int -> VMState) | Output (Int, VMState) | Continue VMState

data StepIO = IOHalt | IOInput (Int -> VMState) | IOOutput (Int, VMState)

explain :: VMState -> String
explain (base, pc, mem) =
  let val i = fromMaybe 0 $ mem Map.!? i
      digit num n = num `div` (10 ^ n) `mod` 10
      l param = case val pc `digit` (param - 1 + 2) of
        0 -> val (pc + param) -- position
        1 -> pc + param -- immediate
        2 -> base + val (pc + param) -- relative
      r = val . l
      pre n =
        let parens s = "(" ++ s ++ ")"
            args =
              if n > 0
                then "args " ++ parens (concat $ take 4 $ (map (printf "%-7d" . val) [pc + 1 .. pc + n + 1]) ++ repeat "       ")
                else ""
         in printf "base %-7d pc %-7d instr %-7d" base pc (val pc) ++ args
   in case val pc `mod` 100 of
        1 -> pre 3 ++ printf " mem[%d] = (mem[%d] (%d) + mem[%d] (%d)) (%d)" (l 3) (l 1) (r 1) (l 2) (r 2) (r 1 + r 2)
        2 -> pre 3 ++ printf " mem[%d] = (mem[%d] (%d) * mem[%d] (%d)) (%d)" (l 3) (l 1) (r 1) (l 2) (r 2) (r 1 * r 2)
        3 -> pre 1 ++ printf " input into mem[%d]" (l 3)
        4 -> pre 0 ++ printf " output mem[%d] (%d)" (l 1) (r 1)
        5 -> pre 2 ++ printf " if mem[%d] (%d) != 0 then pc = mem[%d] (%d) else noop" (l 1) (r 1) (l 2) (r 2)
        6 -> pre 2 ++ printf " if mem[%d] (%d) == 0 then pc = mem[%d] (%d) else noop" (l 1) (r 1) (l 2) (r 2)
        7 -> pre 3 ++ printf " if mem[%d] (%d) < mem[%d] (%d) then mem[%d] = 1 else mem[%d] = 0" (l 1) (r 1) (l 2) (r 2) (l 3) (l 3)
        8 -> pre 3 ++ printf " if mem[%d] (%d) == mem[%d] (%d) then mem[%d] = 1 else mem[%d] = 0" (l 1) (r 1) (l 2) (r 2) (l 3) (l 3)
        9 -> pre 1 ++ printf " add mem[%d] (%d) to base" (l 1) (r 1)
        99 -> ""
        other -> error ("Invalid opcode " ++ show (val pc))

singlestep :: VMState -> SingleStep
singlestep (base, pc, mem) =
  let val i = fromMaybe 0 $ mem Map.!? i
      digit num n = num `div` (10 ^ n) `mod` 10
      l param = case val pc `digit` (param - 1 + 2) of
        0 -> val (pc + param) -- position
        1 -> pc + param -- immediate
        2 -> base + val (pc + param) -- relative
      r = val . l
   in case val pc `mod` 100 of
        1 -> Continue (base, pc + 4, Map.insert (l 3) (r 1 + r 2) mem) -- add
        2 -> Continue (base, pc + 4, Map.insert (l 3) (r 1 * r 2) mem) -- mul
        3 -> Input (\i -> (base, pc + 2, Map.insert (l 1) i mem)) -- input
        4 -> Output (r 1, (base, pc + 2, mem)) -- output
        5 -> Continue (base, (if r 1 /= 0 then r 2 else pc + 3), mem) -- jnz
        6 -> Continue (base, (if r 1 == 0 then r 2 else pc + 3), mem) -- jz
        7 -> Continue (base, pc + 4, Map.insert (l 3) (if r 1 < r 2 then 1 else 0) mem) -- lt
        8 -> Continue (base, pc + 4, Map.insert (l 3) (if r 1 == r 2 then 1 else 0) mem) -- eq
        9 -> Continue (base + r 1, pc + 2, mem) -- adjust base
        99 -> Halt
        other -> error ("Invalid opcode " ++ show (val pc))

stepio :: VMState -> StepIO
stepio state = case singlestep state of
  Halt -> IOHalt
  Input i -> IOInput i
  Output i -> IOOutput i
  Continue state' -> stepio state'

instrument input0 mem0 =
  let recur input state = state : case singlestep state of
        Halt -> []
        Input feed -> recur (tail input) $ feed (head input)
        Output (o, state') -> recur input state'
        Continue state' -> recur input state'
   in recur input0 (0, 0, mem0)

run input0 mem0 =
  let recur input state = case stepio state of
        IOHalt -> []
        IOInput feed -> recur (tail input) $ feed (head input)
        IOOutput (o, state') -> o : recur input state'
   in recur input0 (0, 0, mem0)

runio :: Map.Map Int Int -> ConduitT Int Int IO ()
runio mem0 =
  let recur :: VMState -> ConduitT Int Int IO ()
      recur state = case stepio state of
        IOHalt -> return ()
        IOInput feed -> await >>= \case
          Nothing -> liftIO $ putStrLn "halting, exhausted input"
          Just i -> recur (feed i)
        IOOutput (o, state') -> yield o >> recur state'
   in recur (0, 0, mem0)

runioask :: Map.Map Int Int -> (IO Int) -> IO (IO (Maybe Int))
runioask mem0 ask = do
  stater <- newIORef (0, 0, mem0)
  let loop state = do
        case stepio state of
          IOHalt -> return Nothing
          IOInput feed -> ask >>= loop . feed
          IOOutput (o, state') -> writeIORef stater state' >> return (Just o)
  return (readIORef stater >>= loop)

type VMStateIO = (VMState, Seq Int, Seq Int)

data StopReason = StopInput | StopHalt

emptyVMStateIO mem = ((0, 0, mem), Seq.empty, Seq.empty)

newVM mem = mkVM <$> newSTRef (emptyVMStateIO mem)

mkVM :: STRef s VMStateIO -> (Int -> ST s StopReason, ST s (Maybe Int))
mkVM ref =
  let crank = readSTRef ref >>= \(vm, iseq, oseq) -> case stepio vm of
        IOHalt -> return StopHalt
        IOInput feed -> case Seq.viewl iseq of
          Seq.EmptyL -> return StopInput
          (i Seq.:< iseq') -> writeSTRef ref (feed i, iseq', oseq) >> crank
        IOOutput (o, vm') -> writeSTRef ref (vm', iseq, oseq Seq.|> o) >> crank
      input i = modifySTRef' ref (\(vm, iseq, oseq) -> (vm, iseq Seq.|> i, oseq)) >> crank
      output = do
        crank
        (vm, iseq, oseq) <- readSTRef ref
        case Seq.viewr oseq of
          Seq.EmptyR -> return Nothing
          oseq' Seq.:> o -> Just o <$ writeSTRef ref (vm, iseq, oseq')
   in (input, output)

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

type V2 = (Int, Int)

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
      bestCoords = maximumOn uniqueAngles asteroids
      others = filter (/= bestCoords) asteroids
      -- returns a value that can be used to sort vectors by angle
      angl (0, 0) = error "angl on (0, 0)"
      angl (x, y) =
        let d = gcd x y
         in -- x y -> y -x
            -- but negated y and outer to get: [-pi, pi)
            negate $ atan2 (fromIntegral (x `div` d)) (fromIntegral (y `div` d))
      uniqueAngles coords = length $ uniqueOn angl $ map (`v2minus` coords) $ filter (/= coords) asteroids
      vaporize = map (`v2plus` bestCoords) $ concat $ transpose $ map (sortOn man) $ groupOn angl $ map (`v2minus` bestCoords) others
  putStrLn $ "1a: " ++ show (uniqueAngles bestCoords)
  putStrLn $ "1b: " ++ show (let (x, y) = vaporize !! (200 - 1) in 100 * x + y)

day11 :: IO ()
day11 = do
  in0 <- readFile "input11.txt"
  let prog = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," in0)
  let hull color0 =
        let recur :: VMState -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) () -> (Int, Int) -> Int -> (Map.Map (Int, Int) Int, Map.Map (Int, Int) ())
            recur state h ptd pos dir = case singlestep state of
              Halt -> (h, ptd)
              Input feed -> recur (feed (fromMaybe 0 (h Map.!? pos))) h ptd pos dir
              Output (color, state') -> case singlestep state' of
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

day12 :: IO ()
day12 = do
  in0 <- readFile "input12.txt"
  let parse line = map (fst . head . reads) $ drop 1 $ splitOn "=" line
      sys0 = map (map (,0) . parse) $ lines in0
      energy i =
        (sum . map abs . map fst) i
          * (sum . map abs . map snd) i
      step1 sys =
        let upd (p, v) = let v' = v + sum (map (\(o, _) -> signum (o - p)) sys) in (p + v', v')
         in map upd sys
      period sys1 =
        let (_, poss) =
              until
                (\(sys, seen) -> sys `Set.member` seen)
                (\(sys, seen) -> (step1 sys, Set.insert sys seen))
                (sys1, Set.empty)
         in Set.size poss
  putStrLn $ "1a: " ++ show (sum $ map energy $ transpose $ (!! 1000) $ iterate (map step1) (transpose sys0))
  putStrLn $ "1b: " ++ show (foldl' lcm 1 (map (\axis -> period (map (!! axis) sys0)) [0 .. 2]))

prettyBy :: Map.Map (Int, Int) a -> (Maybe a -> Char) -> String
prettyBy m f = unlines $ do
  y <- [0 .. foldl' max 0 (map snd $ Map.keys m)]
  return $ do
    x <- [0 .. foldl' max 0 (map fst $ Map.keys m)]
    return $ f (m Map.!? (x, y))

day13 :: IO ()
day13 = do
  in0 <- readFile "input13.txt"
  let prog = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," in0)
  putStrLn $ "1a: " ++ show (length $ filter (== 2) $ map (\[_, _, tile] -> tile) $ chunksOf 3 $ run [] prog)
  tilesr <- newIORef Map.empty
  ballxr <- newIORef 0
  paddlexr <- newIORef 0
  let update x y tile = do
        atomicModifyIORef' tilesr (\tiles -> (Map.insert (x, y) tile tiles, ()))
        when (tile == 4) $ writeIORef ballxr x
        when (tile == 3) $ writeIORef paddlexr x
      joystick = repeatMC $ liftIO $ do
        ballx <- readIORef ballxr
        paddlex <- readIORef paddlexr
        return $ signum $ ballx - paddlex
      output = await >>= \case
        Nothing -> return 0
        Just [-1, 0, score] -> do
          tiles <- liftIO $ readIORef tilesr
          if all (/= 2) $ Map.elems tiles
            then return score
            else output
        Just [x, y, tile] -> do
          liftIO $ update x y tile
          output
  score <- runConduit $ joystick .| runio (Map.insert 0 2 prog) .| chunksOfC 3 .| output
  putStrLn $ "1b: " ++ show score

chunksOfC :: Monad m => Int -> ConduitT a [a] m ()
chunksOfC n = if n > 0 then loop n id else error $ "chunksOf size must be positive (given " ++ show n ++ ")"
  where
    loop 0 rest = yield (rest []) >> loop n id
    loop count rest = await >>= \case
      Nothing -> case rest [] of
        [] -> return ()
        nonempty -> yield nonempty
      Just a -> loop (count - 1) (rest . (a :))

type Parser = P.Parsec Void String

binr :: (b -> Bool) -> (Int -> b) -> Int
binr p f =
  let outer n =
        if
          | p (f (2 * n)) -> inner n (2 * n)
          | otherwise -> outer (2 * n)
      inner lo hi =
        let mid = lo + (hi - lo) `div` 2
         in if
              | hi <= lo + 1 -> hi
              | p (f mid) -> inner lo mid
              | otherwise -> inner mid hi
   in outer 1

binl p f = binr p f - 1

day14 :: IO ()
day14 = do
  in0 <- readFile "input14.txt"
  let chemicalP = (,) <$> P.decimal <* " " <*> P.some P.alphaNumChar
      lineP = do
        ls <- chemicalP `P.sepBy1` ", "
        " => "
        (n, pduct) <- chemicalP
        return (pduct, (n, ls))
      parser :: Parser (Map String (Int, [(Int, String)]))
      parser = Map.insert "ORE" (1, []) . Map.fromList <$> (lineP `P.sepBy` P.newline)
  case P.parse parser "" in0 of
    Left e -> putStrLn $ P.errorBundlePretty e
    Right m -> do
      let fetch :: Int -> String -> Map String (Int, Int) -> Map String (Int, Int)
          fetch n chem acc0 =
            let (q, ingreds) = m Map.! chem
                (bought, leftover) = fromMaybe (0, 0) $ (acc0 !? chem)
                (mul, leftover') =
                  if
                    | n <= leftover -> (0, leftover - n)
                    | n > leftover -> let w = (n - leftover + q - 1) `div` q in (w, w * q - (n - leftover))
                z = ((max (n - leftover) 0 + (q - 1)) `div` q) * q
                acc1 = Map.insert chem (bought + mul * q, leftover') acc0
             in foldl'
                  (\acc (need, chm) -> fetch (mul * need) chm acc)
                  acc1
                  ingreds
      putStrLn $ "1a: " ++ show (fst . (! "ORE") $ fetch 1 "FUEL" Map.empty)
      putStrLn $ "1b: " ++ show (binl (>= (10 ^ 12)) (\n -> fst . (! "ORE") $ fetch n "FUEL" Map.empty))

data Dir = U | D | L | R deriving (Eq, Ord, Show)

dirtov2 :: Dir -> V2
dirtov2 U = (0, 1)
dirtov2 D = (0, -1)
dirtov2 L = (-1, 0)
dirtov2 R = (1, 0)

negatedir :: Dir -> Dir
negatedir U = D
negatedir D = U
negatedir L = R
negatedir R = L

var = newSTRef

readr = readSTRef

(.=) = writeSTRef

(.%) = modifySTRef

findpath :: Set V2 -> V2 -> V2 -> Maybe [Dir]
findpath visitable a b = runST $ do
  visitedr <- var (Set.singleton a)
  frontierr <- var (Map.singleton a Seq.empty)
  let loop = readr frontierr >>= \frontier ->
        if Map.null frontier
          then return Nothing
          else case frontier !? b of
            Just path -> return (Just path)
            Nothing -> do
              forM_ (Map.toList frontier) $ \(pos, path) -> do
                frontierr .% Map.delete pos
                let ins dir = do
                      let pos' = pos `v2plus` dirtov2 dir
                      visited <- readr visitedr
                      when (Set.member pos' visitable && Set.notMember pos' visited) $ do
                        frontierr .% Map.insert pos' (path Seq.|> dir)
                        visitedr .% Set.insert pos'
                mapM_ ins [U, D, L, R]
              loop
  fmap toList <$> loop

-- let zero1 = (Set.empty, Map.empty)
--     dirs visitable pos = filter (\dir -> let pos' = pos `v2plus` dir in pos' `Set.member` visitable && pos' `Set.member` visited)
--     step1 zero2 =
--       let step2 (visited, m) =
--       in foldl' step2 zero2 (dirs (Set.member visitable, Set.notMember visited . (`v2plus`)))
--     done (_, m) = b `Map.member` m
--     extract (_, m) = m Map.! b
--  in extract $ until done step1 zero1

day15 :: IO ()
day15 = do
  in0 <- readFile "input2019-15.txt"
  let mem = Map.fromList $ zip [0 ..] (map (read :: String -> Int) $ splitOn "," in0)
      res = runST $ do
        (input, output) <- newVM mem
        curr <- newSTRef (0, 0)
        oxyr <- newSTRef Nothing
        visitabler <- newSTRef (Set.singleton (0, 0))
        toexplorer <- newSTRef (Set.singleton (0, 0))
        exploredr <- newSTRef Set.empty
        let dirtoi dir = fromJust $ lookup dir $ zip [U, D, L, R] [1, 2, 3, 4]
            move dir = do
              input (dirtoi dir)
              output >>= \(Just o) -> do
                when (o /= 0) (curr .% (`v2plus` dirtov2 dir))
                cur <- readr curr
                visitabler .% Set.insert cur
                when (o == 2) (oxyr .= Just cur)
            moveto visitable pos = do
              cur <- readSTRef curr
              let Just path = findpath visitable cur pos
              mapM_ move path
            explore = Set.lookupMin <$> readr toexplorer >>= \case
              Nothing -> return ()
              Just next -> do
                return ()
                toexplorer .% Set.delete next
                visitable <- readr visitabler
                moveto (Set.insert next visitable) next
                exploredr .% Set.insert next
                cur <- readSTRef curr
                when (cur == next) $ do
                  explored <- readr exploredr
                  mapM_ (\n -> toexplorer .% Set.insert n) $ filter (\n -> Set.notMember n explored) $ map (\dir -> cur `v2plus` dirtov2 dir) [U, D, L, R]
                explore
        -- pop 1 toexplore: X
        -- moveto X (consider X visitable)
        -- mark explored
        -- if cur == X, add neighbors of X that are not explored to toexplore
        explore
        visitable <- readr visitabler
        Just oxy <- readr oxyr
        return $ findpath visitable (0, 0) oxy
  -- Just oxy <- readr oxyr
  -- return $ findpath visitable (0, 0) oxy
  print res
  print (length <$> res)
  return ()
