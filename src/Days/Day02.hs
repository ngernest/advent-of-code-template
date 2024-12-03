{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Days.Day02 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------

-- | Parses one single horizontal space character (' ' or '\t')
singleSpaceP :: Parser ()
singleSpaceP = skip isHorizontalSpace

-- | Parses each line as its own list of ints
inputParser :: Parser [[Int]]
inputParser = sepBy1 (sepBy1 decimal singleSpaceP) endOfLine <* endOfInput

------------ PART A ------------

-- | Checks whether an int is within the specified range
diffIsOk :: Int -> Bool
diffIsOk d = absD >= 1 && absD <= 3
  where
    absD = abs d

-- | Checks whether a row is safe by examining `diffs`
-- (the list of differences between successive elements)
isSafe :: [Int] -> Bool
isSafe xs = foldr (\d acc -> diffIsOk d && acc) True diffs && correctSign diffs
  where
    diffs :: [Int]
    diffs = mkDiffs xs

    -- Checks that all the diffs are either all negative or all positive
    correctSign :: [Int] -> Bool
    correctSign diffs = all (< 0) diffs || all (> 0) diffs

-- | Computes the difference between successive elements in a list
mkDiffs :: [Int] -> [Int]
mkDiffs xs = zipWith (-) (tail xs) xs

-- | Computes the no. of safe rows
partA :: [[Int]] -> Int
partA rows = length (filter isSafe rows)

------------ PART B ------------
partB :: [[Int]] -> Int
partB rows = prevAns + length (filter id safetyOfCandidates)
  where
    prevAns :: Int
    prevAns = partA rows

    -- First, find all the rows that are unsafe
    unsafeRows :: [[Int]]
    unsafeRows = filter (not . isSafe) rows

    -- For each row, find all the ways to remove an element from that row
    -- (call each of these resultant lists a "candidate")
    removalCandidates :: [[[Int]]]
    removalCandidates = map allRemovals unsafeRows

    -- For each row, check if there exists a candidate that is now safe
    safetyOfCandidates :: [Bool]
    safetyOfCandidates = map (any isSafe) removalCandidates

-- | All ways to remove an element from a list
allRemovals :: [Int] -> [[Int]]
allRemovals xs = do
  let n = length xs
  i <- [0 .. n - 1]

  -- Compute all possible ways to partition the list at index `i`
  let (prefix, suffix) = splitAt i xs

  return $ prefix ++ drop 1 suffix
