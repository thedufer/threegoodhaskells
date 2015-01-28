module Database where

import Database.PostgreSQL.Simple
import Models

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

rowToMember :: (MemberId, Email, Bool, SendTime, UTCTimestamp) -> Member
rowToMember (id, email, unsubscribed, sendTime, nextEmailDate) = Member id email unsubscribed sendTime nextEmailDate

rowsToMMember :: [FromRow] -> Maybe Member
rowsToMember rows = (liftM rowToMember) (listToMaybe rows)

idToMMember :: Connection -> MemberId -> IO Maybe Member
idToMMember conn id = do
  xs <- query conn "SELECT (id, email, unsubscribed, sendTime, nextEmailDate) FROM Members where id = ?" (Only id)
  return rowsToMMember xs
