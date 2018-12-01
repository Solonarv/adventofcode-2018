module Main where

import System.Environment (getArgs)
import Text.Read

import Solutions

main :: IO ()
main = do
  args <- getArgs
  case args of
    s:ss | Just i <- readMaybe s
         -> do
              case nthSolution i of
                Nothing -> putStrLn $ "No solution for day " <> show i
                Just sln -> do
                  putStrLn $ "Running solution for day " <> show i
                  putStrLn $ "With arguments: " <> show ss
                  sln ss
    _ -> do putStrLn "Running all solutions"; allSolutions []
