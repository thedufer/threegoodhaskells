module DB where

import Models

import Database.PostgreSQL.Simple
import Control.Monad (liftM, void)
import Data.Maybe (listToMaybe)
import Database.PostgreSQL.Simple.Time

{- Wherein we document our tables:
 -
 - Members
 -   id - integer
 -   email - string
 -   unsubscribed - boolean
 -   sendTime - integer
 -   nextEmailDate - timestamp
 -   createdAt - timestamp (unused)
 -   updatedAt - timestamp (unused and not updated since switch to haskell)
 -
 - LoginCodes
 -   id - integer
 -   code - string
 -   expires - timestamp
 -   MemberId - integer
 -   createdAt - timestamp (unused)
 -   updatedAt - timestamp (unused and not updated since switch to haskell)
 -
 - Tokens
 -   id - integer
 -   token - string
 -   MemberId - integer
 -   createdAt - timestamp (unused)
 -   updatedAt - timestamp (unused and not updated since switch to haskell)
 -
 - Posts
 -   id - integer
 -   text - string
 -   date - timestamp (time portion should be 0)
 -   token - string
 -   MemberId - integer
 -   createdAt - timestamp (unused)
 -   updatedAt - timestamp (unused and not updated since switch to haskell)
 -}

insertMember :: Connection -> Member -> IO ()
insertMember conn (Member id email unsubscribed sendTime nextEmailDate) =
  void $ execute conn "INSERT INTO Members (id, email, unsubscribed, sendTime, nextEmailDate) VALUES (?, ?, ?, ?, ?);" (id, email, unsubscribed, sendTime, nextEmailDate)

rowToToken :: (TokenId, String, MemberId) -> Token
rowToToken (id, token, idMember) = Token id token idMember

rowsToMToken :: [(TokenId, String, MemberId)] -> Maybe Token
rowsToMToken = (liftM rowToToken) . listToMaybe

idTokenToMToken :: Connection -> MemberId -> String -> IO (Maybe Token)
idTokenToMToken conn id token = do
  xs <- query conn "SELECT (id, token, MemberId) FROM Tokens WHERE token = ? AND MemberId = ?;" (token, id)
  return (rowsToMToken xs)

rowToMember :: (MemberId, Email, Bool, SendTime, UTCTimestamp) -> Member
rowToMember (id, email, unsubscribed, sendTime, nextEmailDate) = Member id email unsubscribed sendTime nextEmailDate

rowsToMMember :: [(MemberId, Email, Bool, SendTime, UTCTimestamp)] -> Maybe Member
rowsToMMember = (liftM rowToMember) . listToMaybe

idToMMember :: Connection -> MemberId -> IO (Maybe Member)
idToMMember conn id = do
  xs <- query conn "SELECT (id, email, unsubscribed, sendTime, nextEmailDate) FROM Members WHERE id = ?;" (Only id)
  return (rowsToMMember xs)
