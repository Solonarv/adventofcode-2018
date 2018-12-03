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
  , tests =
      [ "+1, -2, +3, +1" :=>
          [ ('a', "3")
          , ('b', "2")
          ]
      , "+1, +1, +1" :=> [ ('a', "3") ]
      , "+1, +1, -2" :=> [ ('a', "0") ]
      , "-1, -2, -3" :=> [ ('a', "-6") ]
      , "+1, -1"             :=> [ ('b', "0") ]
      , "+3, +3, +4, -2, -4" :=> [ ('b', "10") ]
      , "-6, +3, +8, +5, -6" :=> [ ('b', "5") ]
      , "+7, +7, -2, -7, -4" :=> [ ('b', "14") ]
      ]
  }

firstRepeat :: [Int] -> Int
firstRepeat is = go is 0 mempty
  where
    go (x:xs) !acc !seen =
      if acc `IntSet.member` seen
        then acc
        else go xs (acc + x) (IntSet.insert acc seen)
    go [] !acc !seen = go is acc seen
