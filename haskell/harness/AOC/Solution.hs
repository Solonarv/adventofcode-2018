module AOC.Solution where

type Part = Char

data Solution a b = Solution
  { decodeInput :: String -> Maybe a
  , parts       :: [Part]
  , solvePart   :: Part -> a -> Maybe b
  , showResult  :: Part -> b -> String
  , tests       :: [Test]
  }

data Test = String :=> [(Part, String)]