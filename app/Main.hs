{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (Value, decode, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)

-- import Data.ByteString.Lazy.Char8 qualified as B
import Data.Fixed (mod')
import Data.Maybe (fromJust)
import Data.Time (DiffTime, TimeOfDay, defaultTimeLocale, formatTime, parseTimeM, secondsToDiffTime, timeOfDayToTime, timeToTimeOfDay)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest_)
import Network.URI (escapeURIString, isUnreserved)

-- Configuration for the API request
data Config = Config
  { date :: String
  , city :: String
  , country :: String
  , state :: String
  , method :: String
  , apiUrl :: String
  }

defaultConfig :: Config
defaultConfig =
  Config
    { date = ""
    , city = "Pembroke Pines"
    , country = "US"
    , state = ""
    , method = "2"
    , apiUrl = "http://api.aladhan.com/v1/timingsByCity"
    }

-- Data structure for prayer times
data PrayerTimes = PrayerTimes
  { fajr :: String
  , sunrise :: String
  , dhuhr :: String
  , asr :: String
  , maghrib :: String
  , isha :: String
  }
  deriving (Show)

-- Helper function to parse time in HH:mm format
parseTime' :: String -> TimeOfDay
parseTime' = fromJust . parseTimeM True defaultTimeLocale "%H:%M"

-- Helper function to format time in HH:mm format
formatTime' :: TimeOfDay -> String
formatTime' = formatTime defaultTimeLocale "%H:%M"

-- Construct the request URL with parameters
constructUrl :: Config -> String
constructUrl config =
  apiUrl config
    ++ "?date="
    ++ escape (date config)
    ++ "&city="
    ++ escape (city config)
    ++ "&country="
    ++ escape (country config)
    ++ (if null (state config) then "" else "&state=" ++ escape (state config))
    ++ "&method="
    ++ escape (method config)
 where
  escape = escapeURIString isUnreserved

-- Parse the prayer times from the JSON response
parsePrayerTimes :: Value -> Parser PrayerTimes
parsePrayerTimes = withObject "ApiResponse" $ \v -> do
  dataObj <- v .: "data"
  timingsObj <- dataObj .: "timings"
  PrayerTimes
    <$> timingsObj .: "Fajr"
    <*> timingsObj .: "Sunrise"
    <*> timingsObj .: "Dhuhr"
    <*> timingsObj .: "Asr"
    <*> timingsObj .: "Maghrib"
    <*> timingsObj .: "Isha"

-- Get prayer times from the API
getPrayerTimes :: Config -> IO (Maybe PrayerTimes)
getPrayerTimes config = do
  let requestUrl = constructUrl config
  -- putStrLn $ "Request URL: " ++ requestUrl
  response <- httpLBS (parseRequest_ requestUrl)
  -- putStrLn $ "Response: " ++ B.unpack (getResponseBody response)
  let decoded = decode (getResponseBody response) :: Maybe Value
  return $ decoded >>= parseMaybe parsePrayerTimes

-- Calculate the duration of the night in seconds, allowing for durations over 24 hours
nightDurationInSeconds :: PrayerTimes -> DiffTime
nightDurationInSeconds pt
  | endTime >= startTime = endTime - startTime
  | otherwise = endTime - startTime + (24 * 3600)
 where
  startTime = timeOfDayToTime $ parseTime' (maghrib pt)
  endTime = timeOfDayToTime $ parseTime' (fajr pt)

-- Calculate midnight, handling night durations over 24 hours
calculateMidnight :: PrayerTimes -> String
calculateMidnight pt = formatTime' $ timeToTimeOfDay $ (startTime + halfNightDuration) `mod'` secondsInDay
 where
  nightDuration = nightDurationInSeconds pt
  halfNightDuration = nightDuration / 2
  startTime = timeOfDayToTime $ parseTime' (maghrib pt)
  secondsInDay = secondsToDiffTime (24 * 3600)

-- Calculate the end of the first third of the night, handling night durations over 24 hours
calculateFirstThirdEnd :: PrayerTimes -> String
calculateFirstThirdEnd pt = formatTime' $ timeToTimeOfDay $ (startTime + thirdNightDuration) `mod'` secondsInDay
 where
  nightDuration = nightDurationInSeconds pt
  thirdNightDuration = nightDuration / 3
  startTime = timeOfDayToTime $ parseTime' (maghrib pt)
  secondsInDay = secondsToDiffTime (24 * 3600)

-- Calculate the end of the second third of the night, handling night durations over 24 hours
calculateSecondThirdEnd :: PrayerTimes -> String
calculateSecondThirdEnd pt = formatTime' $ timeToTimeOfDay $ (startTime + 2 * thirdNightDuration) `mod'` secondsInDay
 where
  nightDuration = nightDurationInSeconds pt
  thirdNightDuration = nightDuration / 3
  startTime = timeOfDayToTime $ parseTime' (maghrib pt)
  secondsInDay = secondsToDiffTime (24 * 3600)

-- Add a DiffTime to a TimeOfDay, ensuring correct wrapping around 24 hours
addTimeToTimeOfDay :: TimeOfDay -> DiffTime -> TimeOfDay
addTimeToTimeOfDay tod diff = timeToTimeOfDay ((timeOfDayToTime tod + diff) `mod'` secondsInDay)
 where
  secondsInDay = secondsToDiffTime (24 * 3600)

-- Main function to display prayer times and additional calculations
main :: IO ()
main = do
  let config = defaultConfig
  maybePrayerTimes <- getPrayerTimes config
  case maybePrayerTimes of
    Just pt -> do
      putStrLn "Prayer Times \n---------------"
      putStrLn $ "Fajr: " ++ fajr pt
      putStrLn $ "Sunrise: " ++ sunrise pt
      putStrLn $ "Dhuhr: " ++ dhuhr pt
      putStrLn $ "Asr: " ++ asr pt
      putStrLn $ "Maghrib: " ++ maghrib pt
      putStrLn $ "Isha: " ++ isha pt
      putStrLn $ "First Third Of Night End: " ++ calculateFirstThirdEnd pt
      putStrLn $ "Midnight: " ++ calculateMidnight pt
      putStrLn $ "Second Third Of Night End: " ++ calculateSecondThirdEnd pt
    Nothing -> putStrLn "Failed to get prayer times."
