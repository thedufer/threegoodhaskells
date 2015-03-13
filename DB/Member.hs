module DB.Member (newMember, idToMMember, membersNeedEmail, bumpNextEmailDate, emailToMMember, setUnsubscribed, setSendTime) where

import Models
import Time (currentSendTime, sendTimeToNextEmailDate)

import Database.PostgreSQL.Simple
import Control.Monad (liftM, void)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertMember :: Connection -> Member -> IO (Maybe Member)
insertMember conn (Member _ email unsubscribed sendTime nextEmailDate) = do
  curTime <- getCurrentTime
  liftM rowsToMMember $ query conn "INSERT INTO \"Members\" (email, unsubscribed, \"sendTime\", \"nextEmailDate\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?, ?) RETURNING id, email, unsubscribed, \"sendTime\", \"nextEmailDate\";" (email, unsubscribed, sendTime, nextEmailDate, curTime, curTime)

newMember :: Connection -> Email -> IO (Maybe Member)
newMember conn email = do
  sendTime <- currentSendTime
  nextEmailDate <- sendTimeToNextEmailDate sendTime
  insertMember conn (Member undefined email False sendTime (Finite nextEmailDate))

rowToMember :: (MemberId, Email, Bool, SendTime, UTCTimestamp) -> Member
rowToMember (id, email, unsubscribed, sendTime, nextEmailDate) = Member id email unsubscribed sendTime nextEmailDate

rowsToMMember :: [(MemberId, Email, Bool, SendTime, UTCTimestamp)] -> Maybe Member
rowsToMMember = liftM rowToMember . listToMaybe

idToMMember :: Connection -> MemberId -> IO (Maybe Member)
idToMMember conn id = do
  xs <- query conn "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE id = ?;" (Only id)
  return (rowsToMMember xs)

emailToMMember :: Connection -> Email -> IO (Maybe Member)
emailToMMember conn email = do
  xs <- query conn "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE email = ?;" (Only email)
  return (rowsToMMember xs)

membersNeedEmail :: Connection -> IO [Member]
membersNeedEmail conn = do
  curTime <- getCurrentTime
  xs <- query conn "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE \"nextEmailDate\" < ?;" (Only curTime)
  return $ map rowToMember xs

bumpNextEmailDate :: Connection -> Member -> IO ()
bumpNextEmailDate conn (Member idMember _ _ sendTime _) = do
  nextEmailDate <- sendTimeToNextEmailDate sendTime
  execute conn "UPDATE \"Members\" SET \"nextEmailDate\" = ? WHERE id = ?;" (nextEmailDate, idMember)
  return ()

setUnsubscribed :: Connection -> MemberId -> Bool -> IO ()
setUnsubscribed conn idMember unsubscribed =
  void $ execute conn "UPDATE \"Members\" SET unsubscribed = ? WHERE id = ?;" (unsubscribed, idMember)

setSendTime :: Connection -> MemberId -> SendTime -> IO ()
setSendTime conn idMember sendTime = do
  nextEmailDate <- sendTimeToNextEmailDate sendTime
  void $ execute conn "UPDATE \"Members\" SET \"nextEmailDate\" = ?, \"sendTime\" = ? WHERE id = ?;" (nextEmailDate, sendTime, idMember)
