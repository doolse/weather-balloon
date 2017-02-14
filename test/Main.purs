module Test.Main where

import Prelude
import Control.Monad.RWS (execRWS)
import Data.Array (cons, fromFoldable)
import Data.Date (canonicalDate)
import Data.DateTime (DateTime(DateTime))
import Data.Either (Either(..), isLeft)
import Data.Enum (toEnum)
import Data.Foldable (foldr, sequence_)
import Data.Maybe (fromJust, isJust)
import Data.Newtype (unwrap)
import Data.Set (fromFoldable) as Set
import Data.Tuple (fst)
import Math (floor)
import Partial.Unsafe (unsafePartial)
import Test.QuickCheck (class Arbitrary, QC, Result, quickCheck, (<?>), (===))
import Test.QuickCheck.Gen (chooseInt, oneOf, vectorOf)
import Weather (DistanceUnit(..), Measurement(Measurement), TemperatureScale(..), Units, fromKelvin, fromKilometers, parseMeasurement, scaleForObservatory, toKelvin, toKilometers, unitForObservatory)
import Weather.Generate (MostlyReliable, fixMeasurements, mostlyReliableString)
import Weather.Statistics (StatType(Distance, Count, Mean, Max, Min), initialiseStats, statFunc, statString)

newtype StatCollectors = StatCollectors {stat:: StatType, others::Array StatType, units::Units}
instance arbStats :: Arbitrary StatCollectors where
  arbitrary = do
    let arbStatType = oneOf (pure Min) $ pure <$> [Max, Mean, Count, Distance]
    stat <- arbStatType
    n <- chooseInt 0 5
    others <- vectorOf n arbStatType
    tempScale <- oneOf (pure K) [pure F, pure C]
    distUnit <- oneOf (pure Kilometers) [pure Miles]
    pure $ StatCollectors {stat,others,units:{tempScale,distUnit}}

main :: forall e. QC e Unit
main = do
  quickCheck roundTrip
  quickCheck execStats

testEpoch :: DateTime
testEpoch = unsafePartial $ DateTime (fromJust $ canonicalDate <$> toEnum 2017 <*> toEnum 2 <*> toEnum 9) bottom

-- Check that stats remain consistent in the presence of other stats
execStats :: StatCollectors -> Array MostlyReliable -> Result
execStats (StatCollectors {stat, others, units}) mr =
  runStats [stat] === runStats (cons stat others)
  where
    runStats statTypes =
      let statRWS = sequence_ $ statFunc <$> (fromFoldable $ Set.fromFoldable statTypes)
          statResult = (foldr (\m -> fst <<< execRWS statRWS (unwrap m)) initialiseStats (unwrap >>> _.measurement <$> fixMeasurements testEpoch mr))
      in statString units statResult stat

-- Measurement -> String -> Measurement
roundTrip :: MostlyReliable -> Result
roundTrip m = let
    str = mostlyReliableString m
    parsed = parseMeasurement str
    withRounding = rounded (unwrap m).measurement
    success = isLeft parsed && isJust (unwrap m).invalidCharIndex
            || parsed == Right withRounding
  in success <?> "Measurement did not survive the round trip: '" <> str <> "' "
                            <> show withRounding <> ":" <> show parsed

-- Round the measurements because of precision loss of going Number -> Int -> Number
rounded :: Measurement -> Measurement
rounded (Measurement m@{location:{x,y}, kelvin, observatory}) =
  Measurement $ m {location = {x:distRounded x,y:distRounded y},kelvin = tempRounded kelvin}
  where
    scale = scaleForObservatory observatory
    distUnit = unitForObservatory observatory
    tempRounded = toKelvin scale <<< floor <<< fromKelvin scale
    distRounded = toKilometers distUnit <<< floor <<< fromKilometers distUnit
