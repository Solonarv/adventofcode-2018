module Main where

import System.Environment (getArgs)
import Text.Read

import Solutions

main :: IO ()
main = do
  args <- getArgs
  case args of
    s:ss | Just i <- readMaybe s
         , Just sln <- nthSolution i
         -> sln ss
    _ -> allSolutions []