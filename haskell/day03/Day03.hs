module Day03 where

-- import Control.Applicative hiding (many, some)
import Data.Foldable
import qualified Data.Ix as Ix
import Data.Maybe
import Data.Monoid
import Data.Void

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char

import AOC.Solution

solution :: Solution [Claim] Int
solution = Solution
  { decodeInput = parseMaybe parseClaims
  , parts = ['a', 'b']
  , solvePart = \case
      'a' -> Just . countP (> 1) . calcOverlaps
      'b' -> fmap claimId . findLoneClaim
      _ -> const Nothing
  , showResult = \_ -> show
  , tests =
    [ "#123 @ 3,2: 5x4" :=>
        [ ('a', "0")
        ]
    , "#1 @ 1,3: 4x4\n#2 @ 3,1: 4x4\n#3 @ 5,5: 2x2" :=>
        [ ('a', "4")
        , ('b', "3")
        ]
    ]
  }

data Claim = Claim
  { claimId :: !Int
  , claimCorner :: !(V2 Int)
  , claimSize :: !(V2 Int)
  }

type Parser = Parsec Void String

parseClaim :: Parser Claim
parseClaim = do
    _ <- char '#'
    claimId <- int
    space *> char '@' *> space
    claimCorner <- V2 <$> int <* char ',' <*> int
    char ':' *> space
    claimSize <- V2 <$> int <* char 'x' <*> int
    pure Claim{ claimId, claimCorner, claimSize }
  where
    int = read <$> many (digitChar)

parseClaims :: Parser [Claim]
parseClaims = parseClaim `sepEndBy` eol

claimCoords :: Claim -> [V2 Int]
claimCoords (Claim _ corner size) = Ix.range (corner, corner + size - 1)

type ClaimMap = HashMap (V2 Int) Int

addClaim :: ClaimMap -> Claim  -> ClaimMap
addClaim !cmap !claim =
  foldl' (\acc loc -> HashMap.insertWith (+) loc 1 acc)
         cmap
         (claimCoords claim)

calcOverlaps :: Foldable t => t Claim -> ClaimMap
calcOverlaps = foldl' addClaim HashMap.empty

countP :: Foldable t => (a -> Bool) -> t a -> Int
countP p = getSum . foldMap (\case x | p x -> 1; _ -> 0)

findLoneClaim :: [Claim] -> Maybe Claim
findLoneClaim claims = listToMaybe (filter isLoneClaim claims)
  where
    claimMap = calcOverlaps claims
    isLoneClaim c = all (\loc -> loc `HashMap.lookup` claimMap == Just 1) (claimCoords c)