module Time (currentSendTime, sendTimeToNextEmailDate, currentPostDate) where

import Models
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock (getCurrentTime, utctDayTime, addUTCTime, UTCTime)

currentPostDate :: IO UTCTime
currentPostDate = do
  curTime <- getCurrentTime
  curTimeAdj <- return $ addUTCTime (-60 * 60 * 24) curTime
  time <- return $ (fromRational . toRational . negate . utctDayTime) curTimeAdj
  return $ addUTCTime time curTimeAdj

currentSendTime :: IO SendTime
currentSendTime = do
  curTime <- getCurrentTime
  return $ floor $ ((utctDayTime curTime) / 60)

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
