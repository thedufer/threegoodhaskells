module Time (currentSendTime, sendTimeToNextEmailDate, currentPostDate, formatPostDate) where

import Models

import Database.PostgreSQL.Simple.Time
import Data.Time.Clock (getCurrentTime, utctDayTime, addUTCTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

formatPostDate :: UTCTime -> String
formatPostDate = formatTime defaultTimeLocale "%a %h %e %Y"
-- "Sun Feb 22 2015"

currentPostDate :: IO UTCTime
currentPostDate = do
  curTime <- getCurrentTime
  let curTimeAdj = addUTCTime (-60 * 60 * 7) curTime
      time = (fromRational . toRational . negate . utctDayTime) curTimeAdj
  return $ addUTCTime time curTimeAdj

currentSendTime :: IO SendTime
currentSendTime = do
  curTime <- getCurrentTime
  return $ floor $ utctDayTime curTime / 60

sendTimeToNextEmailDate :: SendTime -> IO UTCTime
sendTimeToNextEmailDate st = do
  curTime <- getCurrentTime
  curSendTime <- currentSendTime
  let pNextDate = addUTCTime (realToFrac ((st - curSendTime) * 60)) curTime
    in return $ if pNextDate > curTime
      then pNextDate
      else addUTCTime (60 * 60 * 24) pNextDate

modSendTime :: SendTime -> SendTime
modSendTime st = st `mod` (60 * 24)
