module Solutions where

import Data.Foldable

import qualified Day01
import qualified Day02

type Runnable = [String] -> IO ()

solutions :: [Runnable]
solutions = [ Day01.run
            , Day02.run
            ]

allSolutions :: Runnable
allSolutions args = traverse_ ($ args) solutions

nthSolution :: Int -> Maybe Runnable
nthSolution n = case drop (pred n) solutions of
  [] -> Nothing
  r:_ -> Just r
