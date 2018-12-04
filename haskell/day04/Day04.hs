module Day04 where

import Data.Foldable
import Data.Function
import Data.List (sortOn)

import Data.List.Split
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Text.Megaparsec
import Text.Megaparsec.Char

import AOC.Solution
import Util

solution :: Solution (IntMap [Nap]) [(GuardId, Int)]
solution = Solution
  { decodeInput = fmap napsPerGuard . parseMaybe parseLog
  , parts = ['a']
  , solvePart = \case
      'a' -> Just . findSleepiestGuardAndMinute
      _ -> const Nothing
  , showResult = \case
      'a' -> unwords . fmap (show . uncurry (*))
      _ -> show
  , tests =
      [ unlines
          [ "[1518-11-01 00:00] Guard #10 begins shift"
          , "[1518-11-01 00:05] falls asleep"
          , "[1518-11-01 00:25] wakes up"
          , "[1518-11-01 00:30] falls asleep"
          , "[1518-11-01 00:55] wakes up"
          , "[1518-11-01 23:58] Guard #99 begins shift"
          , "[1518-11-02 00:40] falls asleep"
          , "[1518-11-02 00:50] wakes up"
          , "[1518-11-03 00:05] Guard #10 begins shift"
          , "[1518-11-03 00:24] falls asleep"
          , "[1518-11-03 00:29] wakes up"
          , "[1518-11-04 00:02] Guard #99 begins shift"
          , "[1518-11-04 00:36] falls asleep"
          , "[1518-11-04 00:46] wakes up"
          , "[1518-11-05 00:03] Guard #99 begins shift"
          , "[1518-11-05 00:45] falls asleep"
          , "[1518-11-05 00:55] wakes up"]
        :=> [('a', "240")]
      ]
  }

type Log = [Event]

type GuardId = Int

data Event = BeginShift !Time !GuardId | FallAsleep !Time | WakeUp !Time
  deriving (Show)

evtTime :: Event -> Time
evtTime (BeginShift t _) = t
evtTime (FallAsleep t) = t
evtTime (WakeUp t) = t

data Time = Time { tYear :: !Int, tMonth :: !Int, tDay :: !Int, tHour :: !Int, tMinute :: !Int}
  deriving (Show, Eq, Ord)

parseLog :: Parser Log
parseLog = sortOn evtTime <$> parseEvent `sepEndBy` eol

parseEvent :: Parser Event
parseEvent = do
  time <- parseTime
  space
  (BeginShift time <$ string "Guard #" <*> int <* string " begins shift")
    <|> (FallAsleep time <$ string "falls asleep")
    <|> (WakeUp time <$ string "wakes up")

parseTime :: Parser Time
parseTime = between (char '[') (char ']') do
  tYear   <- int
  tMonth  <- char '-' *> int
  tDay    <- char '-' *> int
  tHour   <- space *> int
  tMinute <- char ':' *> int
  pure Time{ tYear, tMonth, tDay, tHour, tMinute }

data Nap = Nap { napStart :: !Time, napLength :: !Int }
  deriving (Show)

napsPerGuard :: Log -> IntMap [Nap]
napsPerGuard = combineIntMaps . fmap toMap . splitLog
  where
    splitLog = split (dropBlanks . keepDelimsL $ whenElt isBeginShift)
    isBeginShift = \case
      BeginShift{} -> True
      _ -> False
    toMap (BeginShift _ gId : evts) =
      IntMap.singleton gId
        [ Nap beg (tMinute end - tMinute beg + 60 * (tHour end - tHour beg))
        | [FallAsleep beg, WakeUp end] <- chunksOf 2 evts]
    toMap _ = error "can't happen"
    combineIntMaps = foldl' (IntMap.unionWith (<>)) IntMap.empty

findSleepiestGuardAndMinute :: IntMap [Nap] -> [(GuardId, Int)]
findSleepiestGuardAndMinute times = (,) guardId <$> minute
  where
    (guardId, naps) = sleepiestGuard $ times
    minute = mostFrequent $ sleepFreqs naps
    

totalSleepTime :: [Nap] -> Int
totalSleepTime = sum' . fmap napLength

sleepiestGuard :: IntMap [Nap] -> (GuardId, [Nap])
sleepiestGuard = maximumBy (compare `on` (totalSleepTime . snd)) . IntMap.assocs

sleepFreqs :: [Nap] -> FreqMap Int
sleepFreqs = toFreqMap . concatMap napTimes
  where
    napTimes (Nap (Time {tMinute = beg}) len) = [beg .. beg+len-1]