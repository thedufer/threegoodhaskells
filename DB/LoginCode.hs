module DB.LoginCode (insertLoginCode, codeToMLoginCode) where

import Models
import DB

import Control.Monad (liftM)
import Control.Monad.Reader (liftIO)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertLoginCode :: LoginCode -> DatabaseM (Maybe LoginCode)
insertLoginCode (LoginCode _ code expiration idMember) = do
  curTime <- liftIO getCurrentTime
  liftM rowsToMLoginCode $ query "INSERT INTO \"LoginCodes\" (code, expires, \"MemberId\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?) RETURNING id, code, expires, \"MemberId\"" (code, expiration, idMember, curTime, curTime)

rowToLoginCode :: (LoginCodeId, Code, UTCTimestamp, MemberId) -> LoginCode
rowToLoginCode (id, code, expires, idMember) = LoginCode id code expires idMember

rowsToMLoginCode :: [(LoginCodeId, Code, UTCTimestamp, MemberId)] -> Maybe LoginCode
rowsToMLoginCode = liftM rowToLoginCode . listToMaybe

codeToMLoginCode :: Code -> MemberId -> DatabaseM (Maybe LoginCode)
codeToMLoginCode code idMember = do
  curTime <- liftIO getCurrentTime
  xs <- query "SELECT id, code, expires, \"MemberId\" FROM \"LoginCodes\" WHERE code = ? AND \"MemberId\" = ? AND expires > ?;" (code, idMember, curTime)
  return $ rowsToMLoginCode xs
