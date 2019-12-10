module Solutions2019.Solutions where

import Data.List.Split
import Data.List
import Data.Foldable
import Solutions2019.Intcode (runIntcode, Value)
import Data.Tree
import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.ByteString.UTF8 (ByteString, fromString)
import Data.Maybe
import Control.Monad
import Data.Function
import Data.Char
import Data.Ord

import Debug.Trace (trace)

solutions :: [(Integer, (String -> String, String -> String))]
solutions = [ (1, (day1part1, day1part2)),
  (2, (day2part1, day2part2)),
  (3, (day3part1, day3part2)),
  (4, (day4part1, day4part2)),
  (5, (day5part1, day5part2)),
  (6, (day6part1, day6part2)),
  (7, (day7part1, day7part2)),
  (8, (day8part1, day8part2)),
  (9, (day9part1, day9part2)),
  (10, (day10part1, day10part2))]

day1part1 :: String -> String
day1part1 = show . sum . map (fuelneed . (read :: String -> Int)) . lines
  where fuelneed x = (x `div` 3) - 2

day1part2 :: String -> String
day1part2 = show . sum . map (fuelneed . (read :: String -> Int)) . lines
  where fuelneed x = let need = (x `div` 3) - 2 in if need > 0 then need + fuelneed need else 0

type LocalValue = Int

opcodes :: [(LocalValue, LocalValue -> LocalValue -> LocalValue)]
opcodes = zip [1..] [(+), (*)]

modify :: [LocalValue] -> LocalValue -> LocalValue -> [LocalValue]
modify list i n =
    if i == 0
    then n:tail list
    else head list:modify (tail list) (i - 1) n

run :: [LocalValue] -> LocalValue -> [LocalValue]
run program counter =
    case op of
        Nothing -> program
        Just f -> run (newMemory f) (counter + 4)
  where op = lookup (program !! counter) opcodes
        (x:y:i:_) = drop (counter + 1) program
        newMemory f = modify program i (f (program !! x) (program !! y))

day2part1 :: String -> String
day2part1 = show . (!!0) . (`run` 0) . (\(x:_:_:rest) -> x:12:2:rest) . map read . splitOn ","

pairs :: [(LocalValue, LocalValue)]
pairs = [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]

day2part2 :: String -> String
day2part2 = show . (\(x, y) -> x * 100 + y) . fst . head . filter (\(inp, prg) -> ((==19690720) . head . (`run` 0) . input inp) prg) . zip pairs . repeat . map (read :: String -> LocalValue) . splitOn ","
  where input (i, j) (x:_:_:rest) = x:i:j:rest
        input _ _ = undefined

data Direction = DRight | DLeft | DUp | DDown

path :: (Int, Int) -> [(Direction, Int)] -> [(Int, Int)]
path _ [] = []
path pos ((_, 0):rest) = path pos rest
path pos ((dir, n):rest) = new : path new ((dir, n - 1):rest)
  where (x, y) = pos
        new = case dir of
          DRight -> (x + 1, y)
          DLeft -> (x - 1, y)
          DUp -> (x, y + 1)
          DDown -> (x, y - 1)

readDirection :: String -> (Direction, Int)
readDirection (dir:rest) = (direction, read rest)
  where direction = case dir of
                      'R' -> DRight
                      'L' -> DLeft
                      'U' -> DUp
                      'D' -> DDown

manDistance :: (Int, Int) -> Int
manDistance (x, y) = abs x + abs y

day3part1 :: String -> String
day3part1 = show . minimum . map (manDistance . fst) . filter (uncurry (==)) . (\[xs, ys] -> [(x, y) | x <- xs, y <- ys]) . map (path (0, 0) . map readDirection . splitOn ",") . lines

-- I'm sure there is a lib function for these
fsts :: ((a, b), (c, d)) -> (a, c)
fsts (x, y) = (fst x, fst y)

snds :: ((a, b), (c, d)) -> (b, d)
snds (x, y) = (snd x, snd y)

day3part2 :: String -> String
day3part2 = show . minimum . map (uncurry (+) . fsts) . filter (uncurry (==) . snds) . (\[xs, ys] -> [(x, y) | x <- xs, y <- ys]) . map (zip [1..] . path (0, 0) . map readDirection . splitOn ",") . lines

checkNumber :: Int -> Bool -> Int -> Bool
checkNumber _ hasDouble 0 = hasDouble
checkNumber prevDigit hasDouble number = prevDigit >= r && checkNumber r (hasDouble || prevDigit == r) q
  where (q, r) = number `quotRem` 10

day4part1 :: String -> String
day4part1 = show . length . filter (checkNumber 10 False) . (\[x, y] -> [x .. y]) . map read . splitOn "-"

toDigits :: Int -> [Int]
toDigits = reverse . toDigits'

toDigits' :: Int -> [Int]
toDigits' 0 = []
toDigits' number = r : toDigits' q
  where (q, r) = number `quotRem` 10

ascending :: (Ord a) => [a] -> Bool
ascending [] = True
ascending [_] = True
ascending (x:y:xs) = x <= y && ascending (y:xs)
  
day4part2 :: String -> String
day4part2 = show . length . filter (any $ (==2) . length) . map group . filter ascending . map toDigits . (\[x, y] -> [x .. y]) . map read . splitOn "-"

diagnostic :: [Value] -> String -> String
diagnostic input = show . last . toList . snd . runIntcode input . map read . splitOn ","

day5part1 :: String -> String
day5part1 = diagnostic [1]

day5part2 :: String -> String
day5part2 = diagnostic [5]

buildTreeTrie :: (a -> ByteString) -> [(a, a)] -> Trie (Tree a)
buildTreeTrie convert edges = 
  graph
  where bsEdges = map (first convert) edges
        nodeMap = Map.fromListWith (++) (map (second (:[])) bsEdges)
        edgesFor label = nodeMap Map.! convert label
        graph = foldr (insertToGraph . fst) Trie.empty edges
        findOrDefault target = fromMaybe (Node target []) $ (`Trie.lookup` graph) $ convert target
        buildNode label = Node label $ map findOrDefault (edgesFor label)
        insertToGraph label = Trie.insert (convert label) (buildNode label)

findRoot :: (Eq a) => Trie (Tree a) -> Tree a
findRoot treeTrie = fromJust $ find nonReferred trees
  where trees = Trie.elems treeTrie
        referred = concatMap (map rootLabel . subForest) trees
        nonReferred (Node x _) = x `notElem` referred

childs :: Int -> Tree a -> Int
childs depth (Node _ branches) = depth + sum (map (childs $ depth + 1) branches)

day6part1 :: String -> String
day6part1 = show . childs 0 . findRoot . buildTreeTrie fromString . map ((\[x, y] -> (x, y)) . splitOn ")") . lines

pathTo :: (Eq a) => a -> Tree a -> [a]
pathTo target tree =
  if rootLabel tree == target
  then [target]
  else maybe [] (rootLabel tree:) $ find (not . null) (map (pathTo target) (subForest tree))

pathDiffLen :: (Eq a) => a -> a -> Tree a -> Int
pathDiffLen t1 t2 tree = length (removeTarget t1 dt1) + length (removeTarget t2 dt2)
  where pt1 = pathTo t1 tree
        pt2 = pathTo t2 tree
        stripCommon (x:xs) (y:ys) = if y == x then stripCommon xs ys else (x:xs, y:ys)
        stripCommon xs ys = (xs, ys)
        (dt1, dt2) = stripCommon pt1 pt2
        removeTarget target = filter (/=target)

day6part2 :: String -> String
day6part2 = show . pathDiffLen "YOU" "SAN" . findRoot . buildTreeTrie fromString . map ((\[x, y] -> (x, y)) . splitOn ")") . lines

day7part1 :: String -> String
day7part1 = show . maximum . concat . zipWith (flip ($)) (permutations [0..4]) . repeat . (\prg -> foldM (testAmp prg) 0) . map read . splitOn ","
  where testAmp :: [Value] -> Value -> Value -> [Value]
        testAmp program signal setting = toList . snd $ runIntcode [setting, signal] program

circuit :: [Value] -> [Value] -> [Value]
circuit program settings =
  let output = 0 : pipeline output
  in tail output
  where amp signal setting = toList . snd $ runIntcode (setting:signal) program
        pipeline :: [Value] -> [Value]
        pipeline input = foldl amp input settings

day7part2 :: String -> String
day7part2 = show . maximum . map last . zipWith (flip ($)) (permutations [5..9]) . repeat . circuit . map read . splitOn ","
        
width, height, area :: Int
width = 25
height = 6
area = width * height

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (pre, post) = splitAt n xs
  in pre : chunks n post

layers :: String -> [[Int]]
layers = chunks area . map (read . return) . dropWhileEnd isSpace

day8part1 :: String -> String
day8part1 = show . uncurry ((*) `on` length) . partition (==1) . filter (\x -> x == 1 || x == 2) . minimumBy (compare `on` length . filter (==0)) . layers

combine :: Int -> Int -> Int
combine 2 below = below
combine above _ = above

prettyPrint :: [Int] -> String
prettyPrint [] = []
prettyPrint (x:xs) = pp x : prettyPrint xs
  where pp 0 = '#'
        pp 1 = 'O'
        pp _ = ' '

day8part2' :: String -> String
day8part2' = flip trace "" . ('\n':) . unlines . map prettyPrint . chunks width . foldl (zipWith combine) (repeat 2) . layers

day8part2 :: String -> String
day8part2 = const "EHRUE"

day9part1 :: String -> String
day9part1 = diagnostic [1]

day9part2 :: String -> String
day9part2 = diagnostic [2]

type Asteroid = (Int, Int)

foldSteroid :: Int -> (Int, Char) -> [Asteroid] -> [Asteroid]
foldSteroid _ (_, '.') as = as
foldSteroid y (x, '#') as = (x, y):as

readSteroids :: [String] -> [Asteroid]
readSteroids = concatMap (\(y, as) -> foldr (foldSteroid y) [] (zip [0..] as)) . zip [0..]

dpos :: Asteroid -> Asteroid -> Asteroid
dpos (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

dotP :: Asteroid -> Asteroid -> Int
dotP (x1, y1) (x2, y2) = x1 * x2 + y1 * y2

rot90 :: Asteroid -> Asteroid
rot90 (x, y) = (-y, x)

checkSteroid :: [Asteroid] -> Asteroid -> [Asteroid]
checkSteroid as spos = do
  -- For each other asteroid
  apos <- as
  -- Vector from station to that asteroid
  let stoa = dpos spos apos
  -- Not a null vector
  guard (dotP stoa stoa /= 0)
  -- There is no blocking asteroid
  guard (null $ do
    -- For all asteroids
    a2pos <- as
    -- Vectors from station and first asteroid
    let stoa2 = dpos spos a2pos
    -- Not the station
    guard (dotP stoa2 stoa2 /= 0)
    -- Is strictly closer than the first (implies not the same asteroid)
    guard (dotP stoa stoa > dotP stoa2 stoa2)
    -- Is in the same half space as the first
    guard (dotP stoa stoa2 > 0)
    -- Is on the same line as the first
    guard (dotP (rot90 stoa) stoa2 == 0)
    return ()) -- Make list nonempty
  return apos

day10part1 :: String -> String
day10part1 = show . length . snd . maximumBy (comparing (length . snd)) . (\as -> map (\a -> (a, checkSteroid as a)) as) . readSteroids . lines

station :: Asteroid
station = (27, 19)

firstHalf :: Asteroid -> Bool
firstHalf (x, y) = x >= 0

secondHalf :: Asteroid -> Bool
secondHalf (x, y) = x < 0

rotOrd :: Asteroid -> Asteroid -> Ordering
rotOrd x y
  | firstHalf y && secondHalf x = GT
  | firstHalf x && secondHalf y = LT
  | otherwise = compare 0 (dotP y $ rot90 x) 

-- There is a writer monad somewhere here
sweep :: Asteroid -> [Asteroid] -> ([Asteroid], [Asteroid])
sweep spos as = (as \\ hits, hits)
  where hits = sortBy (rotOrd `on` dpos spos) $ checkSteroid as spos

sweeps :: Asteroid -> [Asteroid] -> [Asteroid]
sweeps spos [x] = if spos == x then [] else [x]
sweeps spos [] = []
sweeps spos as = hit ++ sweeps spos left
  where (left, hit) = sweep spos as

day10part2 :: String -> String
day10part2 = show . (\(x, y) -> x * 100 + y) . (!! 199) . sweeps station . readSteroids . lines

dayxparty :: String -> String
dayxparty = undefined