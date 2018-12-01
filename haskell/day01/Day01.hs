module Day01 where

import Data.Foldable

import qualified Data.IntSet as IntSet

run :: [String] -> IO ()
run args = do
  let infile = case args of
        fp:_ | fp /= "." -> fp
        _ -> "day01/input.txt"
  case args of
    _:"sum":_ -> do
      putStr "Sum: "
      readFile infile >>= print . sum' . decode
    _ -> do 
      putStr "First repeat: "
      readFile infile >>= print . firstRepeat . decode

sum' :: [Int] -> Int
sum' = foldl' (+) 0

decode :: String -> [Int]
decode = fmap read . words . filter (`notElem` ",+")

firstRepeat :: [Int] -> Int
firstRepeat is = go is 0 mempty
  where
    go (x:xs) !acc !seen =
      if acc `IntSet.member` seen
        then acc
        else go xs (acc + x) (IntSet.insert acc seen)
    go [] !acc !seen = go is acc seen