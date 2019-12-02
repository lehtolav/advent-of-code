module Solutions2017.Solutions where

import Control.Monad

import Advent
import Data.Char
import qualified Data.Map.Strict as M

solutions :: [(Integer, (String -> String, String -> String))]
solutions = [ (1, (day1part1, day1part2)),
  (2, (day2part1, day2part2)),
  (3, (day3part1, day3part2))]

day1part1 :: String -> String
day1part1 = show . sum . (\x -> zipWith check x (tail $ cycle x)) . map (read . return) . filter isDigit
  where check x y = if x == y then x else (0 :: Int)

day1part2 :: String -> String
day1part2 = show . sum . (\x -> zipWith check x (drop (length x `div` 2) $ cycle x)) . map (read . return) . filter isDigit
  where check x y = if x == y then x else (0 :: Int)

day2part1 :: String -> String
day2part1 = show . sum . map ((\numbers -> maximum numbers - minimum numbers) . map read . words) . lines

day2part2 :: String -> String
day2part2 = show . sum . map (head . onlydivisors . map read . words) . lines

onlydivisors :: [Int] -> [Int]
onlydivisors xs = do
  x <- xs
  y <- xs
  guard $ x /= y
  let (dividend, remainder) = divMod x y
  guard $ remainder == 0
  return dividend

day3part1 :: String -> String
day3part1 = show . (\(x, y) -> abs x + abs y) . day3part1' . read

day3part1' :: Int -> (Int, Int)
day3part1' steps = day3part1'' (steps - 1) 1 (0, 0) DRight

data Direction = DRight | DUp | DLeft | DDown

day3part1'' :: Int -> Int -> (Int, Int) -> Direction -> (Int, Int)
day3part1'' stepsLeft stepSize position direction =
  if stepsLeft < stepSize
  then goDir direction stepsLeft position
  else day3part1'' (stepsLeft - stepSize) (newStepSize direction) (goDir direction stepSize position) (newDirection direction)
  where
  newStepSize DUp = stepSize + 1
  newStepSize DDown = stepSize + 1
  newStepSize _ = stepSize
  newDirection DRight = DUp
  newDirection DUp = DLeft
  newDirection DLeft = DDown
  newDirection DDown = DRight

goDir :: Direction -> Int -> (Int, Int) -> (Int, Int)
goDir DRight n (x, y) = (x + n, y)
goDir DUp n (x, y) = (x, y - n)
goDir DLeft n (x, y) = (x - n, y)
goDir DDown n (x, y) = (x, y + n)

day3part2 :: String -> String
day3part2 = undefined

{-move :: (Int, Int, Direction) -> Direction -> (Int, Int, Direction)
mode (n, maxn, dir) moveDir = case dir of
  Right -> -}