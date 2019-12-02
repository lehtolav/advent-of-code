module Solutions2019.Solutions where

import Control.Monad
import Data.String  
import Debug.Trace  
import Data.List.Split

import Advent

solutions :: [(Integer, (String -> String, String -> String))]
solutions = [ (1, (day1part1, day1part2)),
  (2, (day2part1, day2part2))]

day1part1 :: String -> String
day1part1 = show . sum . map (fuelneed . (read :: String -> Int)) . lines
  where fuelneed x = (x `div` 3) - 2

day1part2 :: String -> String
day1part2 = show . sum . map (fuelneed . (read :: String -> Int)) . lines
  where fuelneed x = let need = (x `div` 3) - 2 in if need > 0 then need + fuelneed need else 0

type Value = Int

opcodes :: [(Value, Value -> Value -> Value)]
opcodes = zip [1..] [(+), (*)]

modify :: [Value] -> Value -> Value -> [Value]
modify list i n =
    if i == 0
    then n:tail list
    else head list:modify (tail list) (i - 1) n

run :: [Value] -> Value -> [Value]
run program counter =
    case op of
        Nothing -> program
        Just f -> run (newMemory f) (counter + 4)
  where op = lookup (program !! counter) opcodes
        (x:y:i:_) = drop (counter + 1) program
        newMemory f = modify program i (f (program !! x) (program !! y))

day2part1 :: String -> String
day2part1 = show . (!!0) . (`run` 0) . (\(x:y:z:rest) -> x:12:2:rest) . map read . splitOn ","

pairs :: [(Value, Value)]
pairs = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

day2part2 :: String -> String
day2part2 = show . (\(x, y) -> x * 100 + y) . fst . head . filter (\(inp, prg) -> ((==19690720) . head . (`run` 0) . input inp) prg) . zip pairs . repeat . map (read :: String -> Value) . splitOn ","
  where input (i, j) (x:y:z:rest) = x:i:j:rest

dayxparty :: String -> String
dayxparty = undefined
