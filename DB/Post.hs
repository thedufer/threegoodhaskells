module DB.Post (newPost) where

import Models
import Time (currentPostDate)
import qualified RandomStrings

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time
import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Data.Time.Clock (getCurrentTime, UTCTime)

rowToPost :: (PostId, Maybe String, UTCTimestamp, PostToken, MemberId) -> Post
rowToPost (id, mText, date, postToken, idMember) = Post id mText date postToken idMember

rowsToMPost :: [(PostId, Maybe String, UTCTimestamp, PostToken, MemberId)] -> Maybe Post
rowsToMPost = (liftM rowToPost) . listToMaybe

dateToPost :: Connection -> UTCTime -> IO (Maybe Post)
dateToPost conn date =
  (liftM rowsToMPost) $ query conn "SELECT (id, text, date, token, \"MemberId\") FROM \"Posts\" WHERE date = ?" (Only date)

insertPost :: Connection -> Post -> IO (Maybe Post)
insertPost conn (Post _ text date token idMember) = do
  curTime <- getCurrentTime
  (liftM rowsToMPost) $ query conn "INSERT INTO \"Posts\" (text, date, token, \"MemberId\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?, ?, ?) RETURNING id, text, date, token, \"MemberId\"" (text, date, token, idMember, curTime, curTime)

newPost :: Connection -> Member -> IO (Maybe Post)
newPost conn member = do
  date <- currentPostDate
  mPrevPost <- dateToPost conn date
  case mPrevPost of
    Just prevPost -> return Nothing
    Nothing -> do
      token <- RandomStrings.generatePostToken
      insertPost conn (Post undefined Nothing (Finite date) token (memberToId member))

idToMPost :: Connection -> PostId -> IO (Maybe Post)
idToMPost conn idPost =
  (liftM rowsToMPost) $ query conn "SELECT (id, text, date, token, \"MemberId\") FROM \"Posts\" WHERE id = ?" (Only idPost)

updateText :: Connection -> PostId -> String -> IO (Maybe Post)
updateText conn idPost text =
  (liftM rowsToMPost) $ query conn "UPDATE \"Posts\" SET text = ? WHERE id = ? RETURNING id, text, date, token, \"MemberId\"" (text, idPost)

addToPost :: Connection -> PostId -> String -> IO (Maybe Post)
addToPost conn idPost text = do
  mPost <- idToMPost conn idPost
  case mPost of
    Nothing -> return Nothing
    Just (Post _ mOldText _ _ _) -> do
      case mOldText of
        Nothing -> updateText conn idPost text
        Just oldText -> updateText conn idPost (oldText ++ "\n" ++ text)
