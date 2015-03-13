module Auth (loadSession, makeLoginCode, makeToken, tokenToCookie) where

import Models
import DB.Member
import DB.LoginCode
import DB.Token
import qualified RandomStrings
import Util (maybeRead)

import qualified Data.Text
import Control.Monad (liftM)
import Web.Cookie (CookiesText, parseCookiesText, SetCookie)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Network.Wai
import Data.List (find)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import Web.Scotty.Cookie (makeSimpleCookie)

doubleBind :: (Monad m) => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
doubleBind f Nothing = return Nothing
doubleBind f (Just x) = f x

loadSession :: Connection -> Request -> IO (Maybe Member)
loadSession conn req = do
  mToken <- doubleBind (uncurry (idTokenToMToken conn)) (reqToMTokenTuple req)
  doubleBind (idToMMember conn) (liftM tokenToMemberId mToken)

reqToCookies :: Request -> CookiesText
reqToCookies req = case find ((== "Cookie") . fst) (requestHeaders req) of
  Nothing -> []
  Just header -> parseCookiesText $ snd header

cookiesToMTokenCookie :: CookiesText -> Maybe Data.Text.Text
cookiesToMTokenCookie = liftM snd . find ((== "token") . fst)

tokenCookieToMTokenTuple :: Data.Text.Text -> Maybe (Data.Text.Text, Data.Text.Text)
tokenCookieToMTokenTuple tc = case Data.Text.splitOn "-" tc of
  [id, token] -> Just (id, token)
  _ -> Nothing

reqToMStringTokenTuple :: Request -> Maybe (Data.Text.Text, Data.Text.Text)
reqToMStringTokenTuple = (tokenCookieToMTokenTuple =<<) . cookiesToMTokenCookie . reqToCookies

stringTokenTupleToMTokenTuple :: (Data.Text.Text, Data.Text.Text) -> Maybe (MemberId, String)
stringTokenTupleToMTokenTuple (a, b) = do
  id <- maybeRead $ Data.Text.unpack a
  let token = Data.Text.unpack b
  return (id, token)

reqToMTokenTuple :: Request -> Maybe (MemberId, String)
reqToMTokenTuple = (stringTokenTupleToMTokenTuple =<<) . reqToMStringTokenTuple

makeLoginCode :: Connection -> MemberId -> IO (Maybe LoginCode)
makeLoginCode conn idMember = do
  code <- RandomStrings.generateCode
  curTime <- getCurrentTime
  let expiration = addUTCTime (60 * 60 * 24) curTime
  insertLoginCode conn (LoginCode undefined code (Finite expiration) idMember)

makeToken :: Connection -> MemberId -> IO (Maybe Token)
makeToken conn idMember = do
  code <- RandomStrings.generateCode
  insertToken conn (Token undefined code idMember)

tokenToCookie :: Token -> SetCookie
tokenToCookie (Token _ code idMember) = makeSimpleCookie "token" $ Data.Text.pack (show idMember ++ "-" ++ code)
