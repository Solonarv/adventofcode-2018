module Main where

import AOC.Harness
import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06

solutions :: Solutions
solutions = solutionsFromList
  [ S Day01.solution
  , S Day02.solution
  , S Day03.solution
  , S Day04.solution
  , S Day05.solution
  , S Day06.solution
  ]

main :: IO ()
main = aocMain 2018 solutions
