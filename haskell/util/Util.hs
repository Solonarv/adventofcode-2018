module Util where

import Control.Monad
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.IORef
import Data.Void
import System.IO.Unsafe

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

-- | Parse an integer.
int :: Parser Int
int = read <$> many digitChar

-- | Appropriately strict version of 'sum'.
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldl' (+) 0

maxIndex :: (Foldable t, Ord a) => t a -> Maybe Int
maxIndex = fmap snd . go . toList
  where
    go [] = Nothing
    go (x : xs) = case go xs of
      Nothing -> Just (x, 0)
      Just (y, i)
        | x >= y    -> Just (x, 0)
        | otherwise -> Just (y, i+1)

newtype FreqMap a = FreqMap { getFreqs :: Map a Int }

instance Ord a => Semigroup (FreqMap a) where
  FreqMap f1 <> FreqMap f2 = FreqMap (Map.unionWith (+) f1 f2)

instance Ord a => Monoid (FreqMap a) where
  mempty = FreqMap Map.empty

toFreqMap :: (Foldable t, Ord a) => t a -> FreqMap a
toFreqMap = FreqMap . Map.fromListWith (+) . fmap (,1) . toList

invert :: FreqMap a -> IntMap [a]
invert = IntMap.fromListWith (<>) . fmap (\(v, f) -> (f, [v])) . Map.assocs . getFreqs

mostFrequent :: FreqMap a -> [a]
mostFrequent = fromMaybe [] . fmap snd . IntMap.lookupMax . invert

totalCount :: FreqMap a -> Int
totalCount = sum' . getFreqs

maximumOn :: (Foldable t, Ord i) => (a -> i) -> t a -> a
maximumOn f = maximumBy (compare `on` f)

minimumOn :: (Foldable t, Ord i) => (a -> i) -> t a -> a
minimumOn f = minimumBy (compare `on` f)

-- | Repeatedly apply a function to an input until
-- a fix-point is reached. May loop forever if no
-- fix-point exists.
fixIterate :: Eq a => (a -> a) -> a -> a
fixIterate f x = if f x == x then x else fixIterate f (f x)

-- | Memoize a function using a @Map@. Uses a mutable reference internally.
-- Note: this is a rather naive implementation. If the map becomes full,
-- it will simply be cleared.
memoMap :: forall a b. Ord a => Int -> (a -> b) -> (a -> b)
memoMap maxSize f = f'
  where
    f' a = unsafePerformIO do
      cache <- readIORef cacheRef
      case Map.lookup a cache of
        Just b -> pure b
        Nothing -> do
          let b = f a
          when (Map.size cache == maxSize) $
            writeIORef cacheRef Map.empty
          modifyIORef cacheRef (Map.insert a b)
          pure b
    cacheRef :: forall s. IORef s (Map a b)
    cacheRef = unsafePerformIO (newIORef Map.empty)
    {-# NOINLINE cacheRef #-}