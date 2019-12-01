module Solutions2019.Solutions where

import Control.Monad
import Data.String  
import Debug.Trace  

import Advent

solutions :: [(Integer, (String -> String, String -> String))]
solutions = [ (1, (day1part1, day1part2))]

day1part1 :: String -> String
day1part1 = show . sum . map (fuelneed . (read :: String -> Int)) . lines
  where fuelneed x = (x `div` 3) - 2

day1part2 :: String -> String
day1part2 = show . sum . map (fuelneed . (read :: String -> Int)) . lines
  where fuelneed x = let need = (x `div` 3) - 2 in if need > 0 then need + fuelneed need else 0