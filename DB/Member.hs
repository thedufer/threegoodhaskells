module DB.Member (newMember, idToMMember) where

import Models
import Time (currentSendTime, sendTimeToNextEmailDate)

import Database.PostgreSQL.Simple
import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertMember :: Connection -> Member -> IO (Maybe Member)
insertMember conn (Member _ email unsubscribed sendTime nextEmailDate) = do
  curTime <- getCurrentTime
  (liftM rowsToMMember) $ query conn "INSERT INTO \"Members\" (email, unsubscribed, \"sendTime\", \"nextEmailDate\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?, ?) RETURNING id, email, unsubscribed, \"sendTime\", \"nextEmailDate\";" (email, unsubscribed, sendTime, nextEmailDate, curTime, curTime)

newMember :: Connection -> Email -> IO (Maybe Member)
newMember conn email = do
  sendTime <- currentSendTime
  nextEmailDate <- sendTimeToNextEmailDate sendTime
  insertMember conn (Member undefined email False sendTime (Finite nextEmailDate))

rowToMember :: (MemberId, Email, Bool, SendTime, UTCTimestamp) -> Member
rowToMember (id, email, unsubscribed, sendTime, nextEmailDate) = Member id email unsubscribed sendTime nextEmailDate

rowsToMMember :: [(MemberId, Email, Bool, SendTime, UTCTimestamp)] -> Maybe Member
rowsToMMember = (liftM rowToMember) . listToMaybe

idToMMember :: Connection -> MemberId -> IO (Maybe Member)
idToMMember conn id = do
  xs <- query conn "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE id = ?;" (Only id)
  return (rowsToMMember xs)
