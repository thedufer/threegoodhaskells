module DB.Token (idTokenToMToken, insertToken) where

import Models
import DB

import Control.Monad (liftM)
import Control.Monad.Reader (liftIO)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertToken :: Token -> DatabaseM (Maybe Token)
insertToken (Token _ code idMember) = do
  curTime <- liftIO getCurrentTime
  liftM rowsToMToken $ query "INSERT INTO \"Tokens\" (token, \"MemberId\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?) RETURNING id, token, \"MemberId\"" (code, idMember, curTime, curTime)

rowToToken :: (TokenId, String, MemberId) -> Token
rowToToken (id, token, idMember) = Token id token idMember

rowsToMToken :: [(TokenId, String, MemberId)] -> Maybe Token
rowsToMToken = liftM rowToToken . listToMaybe

idTokenToMToken :: MemberId -> String -> DatabaseM (Maybe Token)
idTokenToMToken id token = do
  xs <- query "SELECT id, token, \"MemberId\" FROM \"Tokens\" WHERE token = ? AND \"MemberId\" = ?;" (token, id)
  return (rowsToMToken xs)
