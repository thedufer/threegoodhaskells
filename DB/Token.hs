module DB.Token (idTokenToMToken, insertToken) where

import Models

import Database.PostgreSQL.Simple
import Control.Monad (liftM)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time
import Data.Time.Clock

insertToken :: Connection -> Token -> IO (Maybe Token)
insertToken conn (Token _ code idMember) = do
  curTime <- getCurrentTime
  (liftM rowsToMToken) $ query conn "INSERT INTO \"Tokens\" (token, \"MemberId\", \"createdAt\", \"updatedAt\") VALUES (?, ?, ?, ?) RETURNING id, token, \"MemberId\"" (code, idMember, curTime, curTime)

rowToToken :: (TokenId, String, MemberId) -> Token
rowToToken (id, token, idMember) = Token id token idMember

rowsToMToken :: [(TokenId, String, MemberId)] -> Maybe Token
rowsToMToken = (liftM rowToToken) . listToMaybe

idTokenToMToken :: Connection -> MemberId -> String -> IO (Maybe Token)
idTokenToMToken conn id token = do
  xs <- query conn "SELECT id, token, \"MemberId\" FROM \"Tokens\" WHERE token = ? AND \"MemberId\" = ?;" (token, id)
  return (rowsToMToken xs)
