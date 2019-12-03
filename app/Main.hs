module Main where

-- Base libraries
import System.Environment
import System.Timeout
import Control.Monad
import Data.Maybe

-- Third-party libraries
import System.Directory
import Data.Text
import qualified Data.Map as M
import System.Process
import Advent

-- Local solutions module
import Solutions

-- Build defaut options for Advent module
options :: String -> Integer -> AoCOpts
options sessionKey year = defaultAoCOpts year sessionKey

-- Takes exactly 2 arguments and sets timeout to a default of 3s
main :: IO ()
main = do
    [year, day] <- getArgs
    arrangeAndSolve (read year) (read day) 3000000

-- Simple error handling helper to run AoC requests
runAoC' :: AoCOpts -> AoC a -> IO a
runAoC' opts aoc = runAoC opts aoc >>= either (error . show) return

arrangeAndSolve :: Integer -> Integer -> Int -> IO ()
arrangeAndSolve year day timeoutMus = do
  -- Set-up Advent module
  let aocDay = fromMaybe (error ("Invalid day " ++ show day)) $ mkDay day
  let path = "solutionstemp/" ++ show year ++ "/" ++ show day ++ "/"
  sessionKey <- readFile "session-key"
  let aocOptions = (options sessionKey year) { _aCache = Just path }
  let aoc = runAoC' aocOptions

  -- Create folder for year/day if not exists
  createDirectoryIfMissing True path

  -- Show prompt for part 1
  part1Prompt <- (M.! Part1) <$> runAoC' aocOptions (AoCPrompt aocDay)
  showHtml part1Prompt

  -- Fetch input
  input <- unpack <$> aoc (AoCInput aocDay)
  let submit = submitAndProcessResult aocOptions path year day input timeoutMus

  -- Get solution and solve part 1 unless solved and accepted
  part1Success <- submit Part1
  guard part1Success

  -- Print instructions for part 2
  part2Prompt <- (M.! Part2) <$> runAoC' aocOptions (AoCPrompt aocDay)
  showHtml part2Prompt

  -- Get solution and solve part 2 unless solved and accepted
  part2Success <- submit Part2
  guard part2Success
  putStrLn "Both parts were correct!"

-- Submit a solution and handle the server response
submitAndProcessResult :: AoCOpts -> String -> Integer -> Integer -> String -> Int -> Part -> IO Bool
submitAndProcessResult aocOptions basePath year day input timeoutMus part = do
  let aocDay = fromMaybe (error ("Invalid day " ++ show day)) $ mkDay day
  let partPath = basePath ++ show part
  let successPath = partPath ++ ".success"
  let outputPath = partPath ++ ".out"
  isSolved <- doesFileExist successPath
  if isSolved
  then return True
  else do
    putStrLn "No solution yet, calculating..."
    let output = fromMaybe (error "No solution") $ ($input) <$> solution year day part
    -- TODO: As I feared, seems like the timeout is skipped thanks to lazy IO
    fromMaybe (error "No solution") <$> timeout timeoutMus (writeFile outputPath output)
    putStrLn "Calculated, submitting..."
    (response, result) <- runAoC' aocOptions (AoCSubmit aocDay part output)
    putStrLn (showSubmitRes result)
    showHtml response
    if resultFine result
    then writeFile successPath (unpack response) >> return True
    else return False

-- Is our solution correct
resultFine :: SubmitRes -> Bool
resultFine (SubCorrect _) = True
resultFine _ = False

-- Display html using lynx
showHtml :: Text -> IO ()
showHtml html = readProcess "lynx" ["-dump", "-force_html", "-stdin"] (unpack html) >>= putStrLn