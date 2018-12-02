module Day02 where

import Data.Char
import Data.Foldable
import Data.Maybe

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import AOC.Solution

solution :: Solution
solution = Solution
  { decodeInput = Just . lines
  , parts = ['a', 'b']
  , solvePart = \case
      'a' -> Just . Left . checksum
      'b' -> Just . Right . findNeighbors
      _ -> const Nothing
  , showResult = \_ -> \case
      Left chksm -> show chksm
      Right neighbors -> coalesceNeighbors neighbors
  , tests = []
  }

type BoxId = String

c2i :: Char -> Int
c2i = Data.Char.ord

b2i :: Bool -> Int
b2i = fromEnum

countCharOccs :: BoxId -> IntMap Int
countCharOccs = foldl' go IntMap.empty
  where
    go !bag !c = IntMap.insertWith (+) (c2i c) 1 bag

catFromCounts :: IntMap Int -> IntPair
catFromCounts counts = IntPair (b2i $ 2 `elem` counts) (b2i $ 3 `elem` counts)

classifyBoxId :: BoxId -> IntPair
classifyBoxId = catFromCounts . countCharOccs

data IntPair = IntPair !Int !Int

checksum :: [BoxId] -> Int
checksum = mult . foldl' go (IntPair 0 0) . fmap classifyBoxId
  where
    go (IntPair acc2 acc3) (IntPair e2 e3) = IntPair (acc2 + e2) (acc3 + e3)
    mult (IntPair x y) = x * y

close :: BoxId -> BoxId -> Bool
close (a:as) (b:bs)
  | a == b    = close as bs
  | otherwise = as == bs
close [] [] = True
close _ _ = False

findNeighbors :: [BoxId] -> (BoxId, BoxId)
findNeighbors ids = head [(x, y) | x <- ids, y <- filter (/=x) ids, close x y]

coalesceNeighbors :: (BoxId, BoxId) -> BoxId
coalesceNeighbors (x,y) = mapMaybe eqPair $ zip x y
  where
    eqPair (a,b) | a == b    = Just a
                 | otherwise = Nothing