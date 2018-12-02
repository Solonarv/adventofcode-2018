module Day01 where

import Data.Foldable
import Text.Read

import qualified Data.IntSet as IntSet

import AOC.Solution

solution :: Solution
solution = Solution
  { decodeInput = traverse readMaybe . words . filter (`notElem` ",+")
  , parts = ['a', 'b']
  , solvePart = \case
      'a' -> Just . foldl' (+) 0
      'b' -> Just . firstRepeat
      _ -> const Nothing
  , showResult = \_ -> show
  , tests = []
  }

firstRepeat :: [Int] -> Int
firstRepeat is = go is 0 mempty
  where
    go (x:xs) !acc !seen =
      if acc `IntSet.member` seen
        then acc
        else go xs (acc + x) (IntSet.insert acc seen)
    go [] !acc !seen = go is acc seen
