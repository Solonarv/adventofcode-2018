module Main where

import AOC.Harness
import qualified Day01
import qualified Day02

solutions :: Solutions
solutions = solutionsFromList [Day01.solution, Day02.solution]

main :: IO ()
main = aocMain 2018 solutions
