module Weather where

import Prelude
import Data.Bifunctor (bimap)
import Data.Date (day, year)
import Data.DateTime (DateTime(..), hour, minute, month)
import Data.Either (Either(..), fromRight)
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Formatter.DateTime (Formatter, parseFormatString, unformat)
import Data.Formatter.Number (format)
import Data.Int (floor, fromString, toNumber)
import Data.Maybe (Maybe(Nothing), maybe')
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), joinWith, split)
import Partial.Unsafe (unsafePartial)

-- | Distance in kilometers
newtype Coordinate = Coordinate Number
derive newtype instance distanceShw :: Show Coordinate
derive newtype instance distanceEq :: Eq Coordinate
derive instance distNT :: Newtype Coordinate _

-- | Temperature in kelvin
newtype Temperature = Temperature Number
derive newtype instance tempShow :: Show Temperature
derive newtype instance tempEq :: Eq Temperature
derive instance tempNT :: Newtype Temperature _

type Location = {x::Coordinate, y::Coordinate}
data Observatory = AU | US | FR | Other String
derive instance observatoryEq :: Eq Observatory
derive instance observatoryOrd :: Ord Observatory

--- KFC mmmm
data TemperatureScale = K | F | C
data DistanceUnit = Kilometers | Miles

type MeasurementR = {timestamp::DateTime, location::Location, kelvin::Temperature, observatory :: Observatory}
newtype Measurement = Measurement MeasurementR
derive instance measurementEq :: Eq Measurement
derive instance measurementNT :: Newtype Measurement _

type Units = {
    tempScale :: TemperatureScale
  , distUnit :: DistanceUnit
}

instance observatoryShow :: Show Observatory where
  show = observatoryString $ append "Other "

observatoryString :: (String -> String) -> Observatory -> String
observatoryString _ AU = "AU"
observatoryString _ US = "US"
observatoryString _ FR = "FR"
observatoryString f (Other o) = f o

instance measurementShow :: Show Measurement where
  show (Measurement m) = "timestamp: " <> formatDate m.timestamp
                      <> " location: " <> show m.location.x <> ", " <> show m.location.y
                      <> " kelvin: " <> show m.kelvin
                      <> " observatory: " <> show m.observatory

dateFormat :: Formatter
dateFormat = unsafePartial fromRight $ parseFormatString "YYYY-MM-DDTHH:mm"

-- | Parse a temperate measurement entry and normalise to kelvin and kilometers
-- Just use a simple string splitting based parser
parseMeasurement :: String -> Either String Measurement
parseMeasurement s = case split (Pattern "|") s of
  [stamp,locationStr,temp,observeStr] -> do
      let observatory = parseObservatory observeStr
      timestamp <- bimap (\e -> "Could not parse timestamp: " <> stamp <> ": " <> e) id $
                    unformat dateFormat stamp
      kelvin    <- maybeError (\_ -> "Could not parse temperature: " <> temp) $
                    toNumber >>> toKelvin (scaleForObservatory observatory) <$> fromString temp
      location  <- maybeError (\_ -> "Could not parse location: " <> locationStr) $
                    normaliseLocation (unitForObservatory observatory) <$> parseLocation (split (Pattern ",") locationStr)
      pure $ Measurement {timestamp,location,kelvin,observatory}
  _ -> Left $ "Wrong number of components"

  where
    maybeError :: forall a. (Unit -> String) -> Maybe a -> Either String a
    maybeError msg = maybe' (Left <<< msg) Right
    normaliseLocation distUnit {x,y} = let conv = toNumber >>> toKilometers distUnit in {x:conv x, y: conv y}
    parseLocation [xstr,ystr] = {x:_,y:_} <$> fromString xstr <*> fromString ystr
    parseLocation _ = Nothing

parseObservatory :: String -> Observatory
parseObservatory "AU" = AU
parseObservatory "FR" = FR
parseObservatory "US" = US
parseObservatory o = Other o

scaleForObservatory :: Observatory -> TemperatureScale
scaleForObservatory AU = C
scaleForObservatory US = F
scaleForObservatory FR = K
scaleForObservatory (Other _) = K

unitForObservatory :: Observatory -> DistanceUnit
unitForObservatory AU = Kilometers
unitForObservatory US = Miles
unitForObservatory FR = Miles
unitForObservatory (Other _) = Kilometers

toKelvin :: TemperatureScale -> Number -> Temperature
toKelvin s n = Temperature $ case s of
  K -> n
  F -> (n + 459.67) * (5.0/9.0)
  C -> n + 273.15

fromKelvin :: TemperatureScale -> Temperature -> Number
fromKelvin K (Temperature t) = t
fromKelvin F (Temperature t) = t / (5.0/9.0) - 459.67
fromKelvin C (Temperature t) = t - 273.15

toKilometers :: DistanceUnit -> Number -> Coordinate
toKilometers Kilometers = Coordinate
toKilometers Miles = Coordinate <<< mul 1.60934

fromKilometers :: DistanceUnit -> Coordinate -> Number
fromKilometers Kilometers (Coordinate d) = d
fromKilometers Miles (Coordinate d) = d / 1.60934

scaleString :: TemperatureScale -> String
scaleString K = "K"
scaleString F = "F"
scaleString C = "C"

distString :: DistanceUnit -> String
distString Kilometers = "KM"
distString Miles = "M"

-- Write the measurement in the string format using the observatory specific units
measurementString :: Measurement -> String
measurementString m@(Measurement {observatory}) =
  measurementString' {tempScale: scaleForObservatory observatory, distUnit:unitForObservatory observatory} m

-- Write the measurement in the string format using the given units
measurementString' :: Units -> Measurement -> String
measurementString' u (Measurement {timestamp,location:{x,y},observatory,kelvin}) =
  joinWith "|" [
    formatDate timestamp
  , showDist x <> "," <> showDist y
  , show $ floor $ fromKelvin u.tempScale  kelvin
  , observatoryString id observatory
  ]
  where
    showDist = show <<< floor <<< fromKilometers u.distUnit

-- purescript-formatters has a buggy date formatter so
formatDate :: DateTime -> String
formatDate (DateTime d t) = nShow 4 (year d)
                            <> "-" <> nShow 2 (month d)
                            <> "-" <> nShow 2 (day d)
                            <> "T" <> nShow 2 (hour t)
                            <> ":" <> nShow 2 (minute t)
  where
    nShow :: forall a. BoundedEnum a => Int -> a -> String
    nShow before = format {comma:false,before,after:0,abbreviations: false, sign:false} <<< toNumber <<< fromEnum
