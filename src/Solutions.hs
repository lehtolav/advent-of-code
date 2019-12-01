module Solutions
    ( solution
    ) where

import Advent
import qualified Solutions2017.Solutions as AoC2017
import qualified Solutions2019.Solutions as AoC2019

solution :: Integer -> Integer -> Part -> Maybe (String -> String)
solution year day part = selector part <$> lookup day solutions
  where
    solutions = case year of
        2017 -> AoC2017.solutions
        2019 -> AoC2019.solutions
        _    -> []
    selector Part1 = fst
    selector Part2 = snd