module Day06 where

import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char

import AOC.Solution
import Util

solution :: Solution [Point] ()
solution = Solution
  { decodeInput = parseMaybe parsePoints
  , parts = ['a']
  , solvePart = \case
      'a' -> error "TODO"
      _ -> const Nothing
  , showResult = \_ -> show
  , tests =
      [ unlines ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
          :=> [('a', "17")]
      ]
  }

type Point = V2 Int

parsePoints :: Parser [Point]
parsePoints = parsePoint `sepEndBy` eol

parsePoint :: Parser Point
parsePoint = V2 <$> int <* string ", " <*> int