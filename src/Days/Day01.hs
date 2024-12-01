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
inputParser :: Parser [(Int, Int)]
inputParser = sepBy1 (UP.around decimal skipSpace) endOfLine <* endOfInput

------------ PART A ------------

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

partA :: [(Int, Int)] -> Int
partA pairs = sum [abs (x - y) | (x, y) <- zip xs ys]
  where
    (xs, ys) = mapTuple sort (unzip pairs)

------------ PART B ------------
partB :: [(Int, Int)] -> Void
partB = error "Not implemented yet!"
