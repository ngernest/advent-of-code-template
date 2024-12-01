module Days.Day01 (runDay) where

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

import qualified Util.Parsers as UP

import qualified Program.RunDay as R (runDay, Day)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: R.Day
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
-- Note: Attoparsec's `parseOnly` function (in `RunDay.hs`) doesn't make
-- its argument to consume all its input, so we need to post-compose
-- this parser with `endOfInput` at the end
inputParser :: Parser [(Int, Int)]
inputParser = sepBy1 (UP.around decimal skipSpace) endOfLine <* endOfInput

------------ PART A ------------

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

-- | Takes the input list of pairs, and returns a pair of sorted lists
sortedPairs :: [(Int, Int)] -> ([Int], [Int])
sortedPairs pairs = mapTuple sort (unzip pairs)

-- | Computes the sum of the differences of the `i`-th largest element
-- of each list
partA :: [(Int, Int)] -> Int
partA pairs = sum [abs (x - y) | (x, y) <- zip xs ys]
  where
    (xs, ys) = sortedPairs pairs

------------ PART B ------------

-- | Computes the sum of all the elements of `xs`, where each element
-- is multiplied by its no. of occurences in `ys`
partB :: [(Int, Int)] -> Int
partB pairs = Map.foldrWithKey (\x n acc -> x * n + acc) 0 frequencies
  where
    (xs, ys) = sortedPairs pairs

    -- Frequency of each element of `xs` that also appeared in `ys`
    frequencies :: Map Int Int
    frequencies = U.freq (filter (`elem` xs) ys)
