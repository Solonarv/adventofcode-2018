module Day06 where

import Data.Foldable

import Data.Set (Set)
import qualified Data.Set as Set
import Linear.V2
import Text.Megaparsec
import Text.Megaparsec.Char

import AOC.Solution
import Util

solution :: Solution Cloud ()
solution = Solution
  { decodeInput = flay <$> parseMaybe parsePoints
  , parts = ['a']
  , solvePart = \case
      'a' -> error "TODO"
      _ -> const Nothing
  , showResult = \_ -> show
  , tests =
      [ unlines ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
          :=> [('a', "17")]
      ]
  }

type Point = V2 Int

parsePoints :: Parser (Set Point)
parsePoints = Set.fromList <$> parsePoint `sepEndBy` eol

parsePoint :: Parser Point
parsePoint = V2 <$> int <* string ", " <*> int

-- * Convex hull algorithm
-- The points on the convex hull are precisely those with an infinite Voronoi cell.

data Cloud = Cloud { cloudPoints, cloudInterior, cloudBoundary :: Set Point }

-- | Split a point-cloud into points on the boundary and interior points.
flay :: Set Point -> Cloud
flay ps = Cloud ps inner outer
  where
    (inner, outer) = Set.partition onHull ps
    hull = convexHull ps
    hullEdges = zip (cycle hull) (drop 1 hull)
    onHull p = any (p `onEdge`) hullEdges
    p `onEdge` (r1, r2) = (p - r1) `crossZ` (r2 - r1) == 0

-- | Compute the vertices of the given point cloud's convex hull, in order.
convexHull :: Set Point -> [Point]
convexHull ps = (lower ++ upper)
  where
    lower = chain (Set.toAscList ps)
    upper = chain (Set.toDescList ps)

-- | Worker function computing the upper/lower part of the hull
chain :: [Point] -> [Point]
chain = go []
  where
    go acc@(r1:r2:rs) (p:ps)
      | cw r2 r1 p = go (r2:rs) (p:ps)
      | otherwise = go (p:acc) ps
    go acc (p:ps) = go (p:acc) ps
    go acc [] = reverse $ drop 1 acc
    cw o a b = (a - o) `crossZ` (b - o) <= 0

data Tile = TileInCell !Point !Int | TileBorder !Int

largestFiniteCellArea :: Cloud -> Maybe Int
largestFiniteCellArea (Cloud ps inner outer) = _
  where
    grid = memoMap tileOwner
    tileOwner p = asum [circleToTile r (shift p (circle r)) | r <- [0..]]
    circleToTile r circ = Set.minView (circ `Set.intersection` ps) <&> \(o, rest) ->
      if Set.null rest then TileInCell r o else TileBorder r
    

-- | Circle of a specific radius around the origin, in the Manhattan metric.
circle :: Int -> Set Point
circle r = Set.fromList do
  x <- [0..r]
  let y = r - x
  V2 <$> [x, -x] <*> [y, -y]

shift :: Point -> Set Point -> Set Point
shift p = Set.mapMonotonic (p+)