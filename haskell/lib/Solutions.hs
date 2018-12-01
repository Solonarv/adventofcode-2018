module Solutions where

import Data.Foldable

import qualified Day01

type Runnable = [String] -> IO ()

solutions :: [Runnable]
solutions = [Day01.run]

allSolutions :: Runnable
allSolutions args = traverse_ ($ args) solutions

nthSolution :: Int -> Maybe Runnable
nthSolution n = case drop n solutions of
  [] -> Nothing
  r:_ -> Just r