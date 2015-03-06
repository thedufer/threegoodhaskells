module DB.LoginCode (insertLoginCode) where

import Models

import Database.PostgreSQL.Simple
import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertLoginCode :: Connection -> LoginCode -> IO (Maybe LoginCode)
insertLoginCode conn (LoginCode _ code expiration idMember) = do
  curTime <- getCurrentTime
  (liftM rowsToMLoginCode) $ query conn "INSERT INTO \"LoginCodes\" (code, expires, \"MemberId\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?) RETURNING id, code, expires, \"MemberId\"" (code, expiration, idMember, curTime, curTime)

rowToLoginCode :: (LoginCodeId, Code, UTCTimestamp, MemberId) -> LoginCode
rowToLoginCode (id, code, expires, idMember) = LoginCode id code expires idMember

rowsToMLoginCode :: [(LoginCodeId, Code, UTCTimestamp, MemberId)] -> Maybe LoginCode
rowsToMLoginCode = (liftM rowToLoginCode) . listToMaybe
