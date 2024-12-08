{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Eta reduce" #-}
module Days.Day03 (runDay) where

import Control.Arrow (Arrow (second))
import Control.Monad
import Control.Monad.Except
import Data.Attoparsec.Text
import Data.List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text, pack)
import Data.Vector (Vector)
import Data.Vector qualified as Vec
import Data.Void (Void)
import Program.RunDay qualified as R (Day, runDay)
import Text.Regex qualified as R
import Util.Parsers qualified as UP
import Util.Util qualified as U

runDay :: R.Day
runDay = R.runDay inputParser partA partB

regex :: R.Regex
regex = R.mkRegex "mul\\([0-9]+,[0-9]+\\)"

mkMatches :: String -> [Text]
mkMatches s =
  case R.matchRegex regex s of
    Nothing -> []
    Just ms -> map pack ms

parseOnlyMaybe :: Parser a -> Text -> Maybe a
parseOnlyMaybe p s =
  case parseOnly p s of
    Left _ -> Nothing
    Right result -> Just result

-- | Parses the string "mul(x,y)" into the pair of integers `(x, y)`
mulP :: Parser (Int, Int)
mulP = string "mul(" *> UP.around decimal (char ',') <* char ')'

parseMatches :: String -> Maybe [(Int, Int)]
parseMatches s = mapM (parseOnlyMaybe (mulP <* endOfInput)) ms
  where
    ms = mkMatches s

-- >>> R.matchRegexAll regex "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
-- Just ("x","mul(2,4)","%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",[])

------------ PARSER ------------
inputParser :: Parser Input
inputParser = error "Not implemented yet!"

------------ TYPES ------------
type Input = Void

type OutputA = Void

type OutputB = Void

------------ PART A ------------
partA :: Input -> OutputA
partA = error "Not implemented yet!"

------------ PART B ------------
partB :: Input -> OutputB
partB = error "Not implemented yet!"
