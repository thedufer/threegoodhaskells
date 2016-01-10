module DB.Member (newMember, idToMMember, membersNeedEmail, bumpNextEmailDate, emailToMMember, setUnsubscribed, setSendTime, setGotActiveAnnouncement) where

import Models
import DB
import Time (currentSendTime, sendTimeToNextEmailDate)

import Database.PostgreSQL.Simple (Only(..))
import Control.Monad (liftM, void)
import Control.Monad.Reader (liftIO)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertMember :: Member -> DatabaseM (Maybe Member)
insertMember (Member _ email unsubscribed sendTime nextEmailDate) = do
  curTime <- liftIO getCurrentTime
  liftM rowsToMMember $ query "INSERT INTO \"Members\" (email, unsubscribed, \"sendTime\", \"nextEmailDate\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?, ?) RETURNING id, email, unsubscribed, \"sendTime\", \"nextEmailDate\";" (email, unsubscribed, sendTime, nextEmailDate, curTime, curTime)

newMember :: Email -> DatabaseM (Maybe Member)
newMember email = do
  sendTime <- liftIO currentSendTime
  nextEmailDate <- liftIO $ sendTimeToNextEmailDate sendTime
  insertMember (Member undefined email False sendTime (Finite nextEmailDate))

rowToMember :: (MemberId, Email, Bool, SendTime, UTCTimestamp) -> Member
rowToMember (id, email, unsubscribed, sendTime, nextEmailDate) = Member id email unsubscribed sendTime nextEmailDate

rowsToMMember :: [(MemberId, Email, Bool, SendTime, UTCTimestamp)] -> Maybe Member
rowsToMMember = liftM rowToMember . listToMaybe

idToMMember :: MemberId -> DatabaseM (Maybe Member)
idToMMember id = do
  xs <- query "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE id = ?;" (Only id)
  return (rowsToMMember xs)

emailToMMember :: Email -> DatabaseM (Maybe Member)
emailToMMember email = do
  xs <- query "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE email = ?;" (Only email)
  return (rowsToMMember xs)

membersNeedEmail :: DatabaseM [Member]
membersNeedEmail = do
  curTime <- liftIO getCurrentTime
  xs <- query "SELECT id, email, unsubscribed, \"sendTime\", \"nextEmailDate\" FROM \"Members\" WHERE \"nextEmailDate\" < ? AND unsubscribed = FALSE;" (Only curTime)
  return $ map rowToMember xs

bumpNextEmailDate :: Member -> DatabaseM ()
bumpNextEmailDate (Member idMember _ _ sendTime _) = do
  nextEmailDate <- liftIO $ sendTimeToNextEmailDate sendTime
  execute "UPDATE \"Members\" SET \"nextEmailDate\" = ? WHERE id = ?;" (nextEmailDate, idMember)
  return ()

setUnsubscribed :: MemberId -> Bool -> DatabaseM ()
setUnsubscribed idMember unsubscribed =
  void $ execute "UPDATE \"Members\" SET unsubscribed = ? WHERE id = ?;" (unsubscribed, idMember)

setSendTime :: MemberId -> SendTime -> DatabaseM ()
setSendTime idMember sendTime = do
  nextEmailDate <- liftIO $ sendTimeToNextEmailDate sendTime
  void $ execute "UPDATE \"Members\" SET \"nextEmailDate\" = ?, \"sendTime\" = ? WHERE id = ?;" (nextEmailDate, sendTime, idMember)

setGotActiveAnnouncement :: Member -> DatabaseM ()
setGotActiveAnnouncement (Member idMember _ _ _ _) =
  void $ execute "UPDATE \"Members\" SET \"gotActiveAnnouncement\" = true WHERE id = ?;" (Only idMember)
