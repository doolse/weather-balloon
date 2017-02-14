module Weather.Statistics where

import Prelude
import Control.Alt ((<|>))
import Control.Monad.RWS (RWS, get, modify, tell)
import Control.Monad.Reader (ask, asks)
import Data.DateTime (DateTime)
import Data.Formatter.Number (formatOrShowNumber)
import Data.Int (toNumber)
import Data.Lens (Lens', Setter', lens, modifying)
import Data.Lens.At (at)
import Data.Map (Map, empty, toAscUnfoldable)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (over2)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))
import Math (sqrt)
import Weather (Coordinate(Coordinate), Location, Measurement(Measurement), MeasurementR, Observatory, Temperature(Temperature), Units, distString, fromKelvin, fromKilometers, scaleString)

-- Stats are calculated with RWS operations on this record
-- The Writer type is an array of warnings
type Stats = {
    minimum :: Temperature
  , maximum :: Temperature
  , mean :: Maybe Temperature
  , observationCount:: Map Observatory Int
  , lastTime :: DateTime
  , lastLocation :: Location
  , distance :: Coordinate
}

type StatCalc = RWS MeasurementR (Array String) Stats Unit

data StatType = Min | Max | Mean | Count | Distance
derive instance statTypeEq :: Eq StatType
derive instance statTypeOrd :: Ord StatType

initialiseStats :: Stats
initialiseStats = {
    minimum: Temperature $ toNumber top
  , maximum: Temperature $ toNumber bottom
  , mean: Nothing
  , observationCount: empty
  , lastTime: bottom
  , lastLocation: {x:Coordinate 0.0, y:Coordinate 0.0}
  , distance: Coordinate 0.0
}

_minimum :: Lens' Stats Temperature
_minimum = lens _.minimum _{minimum=_}

_maximum :: Lens' Stats Temperature
_maximum = lens _.maximum _{maximum=_}

_observationCount :: Lens' Stats (Map Observatory Int)
_observationCount = lens _.observationCount _{observationCount=_}

-- Perform a binary op over the temperature
tempOp :: (Number -> Number -> Number) -> (Setter' Stats Temperature) -> StatCalc
tempOp op l = asks _.kelvin >>= (modifying l <<< over2 Temperature op)

minStat :: StatCalc
minStat = tempOp min _minimum

maxStat :: StatCalc
maxStat = tempOp max _maximum

-- Calculating the mean like this might introduce precision errors
meanStat :: StatCalc
meanStat = do
    {kelvin} <- ask
    modify \s -> s { mean = (over2 Temperature avg kelvin <$> s.mean) <|> Just kelvin }
  where avg x y = (x + y) * 0.5

countStat :: StatCalc
countStat = do
  {observatory} <- ask
  modifying (at observatory >>> _observationCount) $ Just <<< fromMaybe 0 >>> ((+) 1)

-- Out of order timestamps are ignored apart from a warning being logged
distStat :: StatCalc
distStat = do
  m@{location:loc@{x:(Coordinate x),y:(Coordinate y)},timestamp} <- ask
  s@{lastLocation, lastTime, distance:(Coordinate d)} <- get
  if lastTime < timestamp
    then modify _ {lastTime=timestamp, lastLocation=loc, distance= Coordinate $ d + (sqrt $ x*x + y*y)}
    else tell ["Out of order timestamp: " <> show (Measurement m)]

statFunc :: StatType -> StatCalc
statFunc Min = minStat
statFunc Max = maxStat
statFunc Mean = meanStat
statFunc Count = countStat
statFunc Distance = distStat

-- Produce a readable string with the appropriate units
statString :: Units -> Stats -> StatType -> String
statString u stats = case _ of
  Min -> "Minimum temperature: " <> tempValue stats.minimum
  Max -> "Maximum temperature: " <> tempValue stats.maximum
  Mean -> "Mean temperature: " <> fromMaybe "<no stat>" (tempValue <$> stats.mean)
  Count -> "Counts per observatory:\n" <> (joinWith "\n" $ printCount <$> toAscUnfoldable stats.observationCount)
  Distance -> "Total distance travelled: " <> (formatOrShowNumber "0.0" $ fromKilometers u.distUnit stats.distance)
                                           <> " " <> distString u.distUnit
  where
    printCount (Tuple o c) = " - " <> show o <> " " <> show c
    tempValue v = formatOrShowNumber "0.00" (fromKelvin u.tempScale v) <> " " <> scaleString u.tempScale
