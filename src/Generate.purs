module Weather.Generate where

import Prelude
import Data.Array (length, scanl, updateAt, zipWith)
import Data.DateTime (DateTime, adjust)
import Data.DateTime.Instant (instant, toDateTime)
import Data.Int (toNumber)
import Data.List (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Ord (abs)
import Data.String (fromCharArray, toCharArray)
import Data.Time.Duration (Milliseconds(Milliseconds), Minutes(..))
import Data.Tuple (Tuple(..))
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (chooseInt, frequency)
import Weather (Coordinate(..), Location, Measurement(Measurement), Observatory(Other, FR, US, AU), Temperature(Temperature), measurementString)


-- Difference in minutes from previous measurement
newtype TimeDiff = TimeDiff Minutes
derive instance timeDiffNT :: Newtype TimeDiff _

-- A mostly reliable weather measurement
-- Keeps a time difference value and optional invalid char index so we
-- can later break the data
newtype MostlyReliable = MostlyReliable {timeDiff::TimeDiff, invalidCharIndex :: Maybe Int, measurement::Measurement}
derive instance mostlyNT :: Newtype MostlyReliable _

newtype ArbLocation = ArbLocation Location
newtype ArbTemperature = ArbTemperature Temperature
newtype ArbObservatory = ArbObservatory Observatory
newtype ArbCoordinate = ArbDistance Coordinate

--
instance arbDistance :: Arbitrary ArbCoordinate where
  arbitrary = ArbDistance <<< Coordinate <<< toNumber <$> chooseInt 0 15000

instance arbLocation :: Arbitrary ArbLocation where
  arbitrary = do
    (ArbDistance x) <- arbitrary
    (ArbDistance y) <- arbitrary
    pure $ ArbLocation {x,y}

instance arbTemp :: Arbitrary ArbTemperature where
  arbitrary = ArbTemperature <<< Temperature <<< toNumber <$> chooseInt 220 400

instance arbObs :: Arbitrary ArbObservatory where
  arbitrary = do
    i <- chooseInt 0 4
    pure $ ArbObservatory $ case i of
      0 -> AU
      1 -> US
      2 -> FR
      3 -> Other $ "UK"
      o -> Other $ "Somewhere" <> show o

instance arbTimeDiff :: Arbitrary TimeDiff where
  arbitrary = TimeDiff <<< Minutes <<< toNumber <$> chooseInt (-2) 9

instance arbitraryMeasurement :: Arbitrary MostlyReliable where
  arbitrary = do
    timestampM <- arbitrary
    let timestamp = fromMaybe bottom $ toDateTime <$> instant (Milliseconds timestampM)
    (ArbLocation location) <- arbitrary
    (ArbTemperature kelvin) <- arbitrary
    (ArbObservatory observatory) <- arbitrary
    timeDiff <- arbitrary
    invalidCharIndex <- frequency (Tuple 0.90 $ pure Nothing) $ singleton $ Tuple 0.10 $ (Just <$> arbitrary)
    pure $ MostlyReliable {timeDiff, invalidCharIndex, measurement: Measurement {timestamp, location, kelvin, observatory}}

-- Write a mostly reliable string by possibly changing a char into a '|' character
mostlyReliableString :: MostlyReliable -> String
mostlyReliableString (MostlyReliable {invalidCharIndex,measurement}) =
  let str = measurementString measurement
  in fromMaybe str do
    ind <- invalidCharIndex
    let chars = toCharArray str
    fromCharArray <$> updateAt (abs $ mod ind $ length chars) '|' chars

-- Adjust the times of an Array of measurements by using the time difference to find the next
-- time.
fixMeasurements :: DateTime -> Array MostlyReliable -> Array MostlyReliable
fixMeasurements epoch m = zipWith adjustTime m (scanl stamp epoch m)
  where
    stamp t (MostlyReliable {timeDiff}) = fromMaybe t $ adjust (unwrap timeDiff) t
    adjustTime (MostlyReliable mr) t = MostlyReliable mr {measurement = over Measurement _ {timestamp=t} mr.measurement}
