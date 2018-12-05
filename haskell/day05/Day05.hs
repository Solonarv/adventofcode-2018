module Day05 where

import Data.Char
import Data.Maybe

import AOC.Solution
import Util

solution :: Solution Polymer Polymer
solution = Solution
  { decodeInput = Just . mapMaybe toMonomer
  , parts = ['a', 'b']
  , solvePart = \case
      'a' -> Just . reactFully
      'b' -> Just . shortestAfterRemoving . reactFully
      _ -> const Nothing
  , showResult = \case
      'a' -> show . length
      'b' -> show . length
      _ -> show
  , tests =
    [ "abBA" :=> [('a', "0")]
    , "abAB" :=> [('a', "4")]
    , "aabAAB" :=> [('a', "6")]
    , "dabAcCaCBAcCcaDA" :=> [('a', "10"), ('b', "4")]
    ]
  }

-- meaning: < 26 -> lowercase, >= 26 -> uppercase
newtype Monomer = Mon Int
  deriving (Eq, Ord)

instance Show Monomer where
  showsPrec _ m = (monomerToChar m:)
  showList = showString . showPolymer

toMonomer :: Char -> Maybe Monomer
toMonomer ch
  | ch >= 'a' && ch <= 'z'
  = Just (Mon (ord ch - ord 'a'))
  | ch >= 'A' && ch <= 'Z'
  = Just (Mon (ord ch - ord 'A' + 26))
  | otherwise
  = Nothing

monomerToChar :: Monomer -> Char
monomerToChar (Mon i)
  | i < 26
  = chr (i + ord 'a')
  | otherwise
  = chr (i - 26 + ord 'A')

canReact :: Monomer -> Monomer -> Bool
canReact (Mon x) (Mon y) = abs (x - y) == 26

type Polymer = [Monomer]

showPolymer :: Polymer -> String
showPolymer = fmap monomerToChar

reactOnce :: Polymer -> Polymer
reactOnce (x:y:rest)
  | canReact x y = reactOnce rest
  | otherwise = x : reactOnce (y : rest)
reactOnce ms = ms

reactFully :: Polymer -> Polymer
reactFully = fixIterate reactOnce

shortestAfterRemoving :: Polymer -> Polymer
shortestAfterRemoving = shortest . tryAllRemovals
  where shortest = minimumOn length

tryAllRemovals :: Polymer -> [Polymer]
tryAllRemovals p = [ reactFully $ filter (not . matches rm) p | rm <- Mon <$> [0..25] ]
  where
    matches (Mon x) (Mon y) = (x - y) `mod` 26 == 0