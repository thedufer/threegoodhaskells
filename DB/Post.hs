module DB.Post (newPost, memberToPosts, addToPost, dateToPost) where

import Models
import DB
import Time (currentPostDate)
import qualified RandomStrings

import Database.PostgreSQL.Simple (Only(..))
import Database.PostgreSQL.Simple.Time
import Control.Monad (liftM)
import Control.Monad.Reader (liftIO)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime)

rowToPost :: (PostId, Maybe String, UTCTimestamp, PostToken, MemberId) -> Post
rowToPost (id, mText, date, postToken, idMember) = Post id mText date postToken idMember

rowsToMPost :: [(PostId, Maybe String, UTCTimestamp, PostToken, MemberId)] -> Maybe Post
rowsToMPost = liftM rowToPost . listToMaybe

dateToPost :: MemberId -> UTCTime -> DatabaseM (Maybe Post)
dateToPost idMember date =
  liftM rowsToMPost $ query "SELECT id, text, date, token, \"MemberId\" FROM \"Posts\" WHERE date = ? AND \"MemberId\" = ?" (date, idMember)

insertPost :: Post -> DatabaseM (Maybe Post)
insertPost (Post _ text date token idMember) = do
  curTime <- liftIO getCurrentTime
  liftM rowsToMPost $ query "INSERT INTO \"Posts\" (text, date, token, \"MemberId\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?, ?) RETURNING id, text, date, token, \"MemberId\"" (text, date, token, idMember, curTime, curTime)

newPost :: Member -> DatabaseM (Maybe Post)
newPost member = do
  date <- liftIO currentPostDate
  mPrevPost <- dateToPost (memberToId member) date
  case mPrevPost of
    Just prevPost -> return Nothing
    Nothing -> do
      token <- liftIO RandomStrings.generatePostToken
      insertPost (Post undefined Nothing (Finite date) token (memberToId member))

idToMPost :: PostId -> DatabaseM (Maybe Post)
idToMPost idPost =
  liftM rowsToMPost $ query "SELECT id, text, date, token, \"MemberId\" FROM \"Posts\" WHERE id = ?" (Only idPost)

tokenToMPost :: PostToken -> DatabaseM (Maybe Post)
tokenToMPost postToken =
  liftM rowsToMPost $ query "SELECT id, text, date, token, \"MemberId\" FROM \"Posts\" WHERE token = ?" (Only postToken)

updateText :: PostId -> String -> DatabaseM (Maybe Post)
updateText idPost text =
  liftM rowsToMPost $ query "UPDATE \"Posts\" SET text = ? WHERE id = ? RETURNING id, text, date, token, \"MemberId\"" (text, idPost)

addToPost :: PostToken -> String -> DatabaseM (Maybe Post)
addToPost postToken text = do
  mPost <- tokenToMPost postToken
  case mPost of
    Nothing -> return Nothing
    Just (Post idPost mOldText _ _ _) ->
      case mOldText of
        Nothing -> updateText idPost text
        Just oldText -> updateText idPost (oldText ++ "\n" ++ text)

memberToPosts :: Member -> DatabaseM [Post]
memberToPosts (Member idMember _ _ _ _) =
  liftM (map rowToPost) $ query "SELECT id, text, date, token, \"MemberId\" FROM \"Posts\" WHERE \"MemberId\" = ?" (Only idMember)
