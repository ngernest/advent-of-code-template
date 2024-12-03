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

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: [[Int]] -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: [[Int]] -> OutputB
partB = error "Not implemented yet!"
