module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.RWS (execRWS)
import Control.Monad.ST (newSTRef, readSTRef, runST, writeSTRef)
import Control.Safely (traverse_)
import Data.Array (foldRecM, last, mapMaybe, range)
import Data.DateTime.Instant (toDateTime)
import Data.Either (either)
import Data.Foldable (sequence_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(Tuple))
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Stream (createReadStream)
import Node.Path (FilePath)
import Node.Process (PROCESS, argv)
import Node.Stream (Readable, onDataString, onEnd)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (randomSample')
import Weather (DistanceUnit(..), Measurement(..), TemperatureScale(..), Units, measurementString', parseMeasurement)
import Weather.Generate (fixMeasurements, mostlyReliableString)
import Weather.Statistics (StatType(..), initialiseStats, statFunc, statString)

foreign import bylineStream :: forall r e. Readable r e -> Eff e (Readable r e)

main :: forall eff. Eff (process::PROCESS, now::NOW, random::RANDOM, err::EXCEPTION, console::CONSOLE, fs::FS|eff) Unit
main = do
    args <- argv
    case args of
      [_, _, "stats", statTypes, tempScale, distUnit, filename] -> execStats
                                      (mapMaybe parseType $ split (Pattern ",") statTypes)
                                      (parseUnits tempScale distUnit) filename
      [_, _, "generate", thousands] -> generateTestData $ fromMaybe 1000 $ fromString thousands
      [_, _, "normalise", tempScale, distUnit, filename] -> normaliseData (parseUnits tempScale distUnit) filename
      _ -> error $ "Usage:\n"
            <> "stats min|max|mean|count|dist K|F|C KM|M filename\n"
            <> "generate thousands\n"
            <> "normalise K|F|C KM|M filename\n"
  where
    parseUnits tempScale distUnit = {tempScale:parseTempScale tempScale, distUnit: parseTempUnit distUnit}
    parseType "min" = Just Min
    parseType "max" = Just Max
    parseType "mean" = Just Mean
    parseType "count" = Just Count
    parseType "dist" = Just Distance
    parseType _ = Nothing
    parseTempScale "K" = K
    parseTempScale "F" = F
    parseTempScale _ = C
    parseTempUnit "M" = Miles
    parseTempUnit _ = Kilometers

-- | Read weather measurements and collect statistics
--
-- Streaming is done with a Node readable filesystem stream which exposes an event callback based API
-- The statistics state is kept in a locally mutable reference so that the callbacks can access it
--
execStats :: forall eff. Array StatType -> Units -> FilePath -> Eff (process::PROCESS, err::EXCEPTION, console::CONSOLE, fs::FS|eff) Unit
execStats statTypes units filename = runST (do
  stream <- createReadStream filename >>= bylineStream
  state <- newSTRef initialiseStats

  let statsCollector = sequence_ $ statFunc <$> statTypes
      processMeasurement (Measurement m) = do
        (Tuple next errs) <- execRWS statsCollector m <$> readSTRef state
        writeSTRef state next
        traverse_ error errs
      processLine str = either (error <<< flip append (" - " <> str)) processMeasurement $ parseMeasurement str

  onDataString stream UTF8 processLine
  
  -- Write out the stats
  onEnd stream do
    stats <- readSTRef state
    traverse_ log $ (statString units) stats <$> statTypes
)

-- | Generate mostly reliable test data using QuickCheck
generateTestData :: forall e. Int -> Eff( now :: NOW, random :: RANDOM, console :: CONSOLE | e) Unit
generateTestData thousands = do
    startStamp <- toDateTime <$> now
    void $ foldRecM writeBatch startStamp (range 0 (thousands - 1))
  where
  -- Generate the data in batches of 1000
  writeBatch stamp _ = do
    samples <- fixMeasurements stamp <$> randomSample' 1000 arbitrary
    traverse_ (log <<< mostlyReliableString) samples
    -- return the last stamp so we can continue from there
    pure $ fromMaybe stamp (unwrap >>> _.measurement >>> unwrap >>> _.timestamp <$> last samples)

normaliseData :: forall eff. Units -> String -> Eff (process::PROCESS, err::EXCEPTION, console::CONSOLE, fs::FS|eff) Unit
normaliseData units filename = runST (do
  stream <- createReadStream filename >>= bylineStream
  let processLine str = either error (log <<< measurementString' units) $ parseMeasurement str
  onDataString stream UTF8 processLine
)
