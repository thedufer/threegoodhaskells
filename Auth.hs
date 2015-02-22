module Auth (loadSession, makeLoginCode) where

import Models
import DB.Member
import DB.LoginCode
import DB.Token

import qualified Data.Text
import Control.Monad (liftM, replicateM)
import Web.Cookie (CookiesText, parseCookiesText)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Network.Wai
import Data.List (find)
import Data.Maybe (listToMaybe)
import Numeric (showHex)
import Data.Time.Clock (getCurrentTime, addUTCTime)
import System.Random (randomRIO)

doubleBind :: (Monad m) => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
doubleBind f Nothing = return Nothing
doubleBind f (Just x) = f x

loadSession :: Connection -> Request -> IO (Maybe Member)
loadSession conn req = do
  mToken <- doubleBind (uncurry (idTokenToMToken conn)) (reqToMTokenTuple req)
  doubleBind (idToMMember conn) ((liftM tokenToMemberId) mToken)

reqToCookies :: Request -> CookiesText
reqToCookies req = case (find ((== "Cookie") . fst) (requestHeaders req)) of
  Nothing -> []
  Just header -> parseCookiesText $ snd header

cookiesToMTokenCookie :: CookiesText -> Maybe Data.Text.Text
cookiesToMTokenCookie = (liftM snd) . (find ((== "token") . fst))

tokenCookieToMTokenTuple :: Data.Text.Text -> Maybe (Data.Text.Text, Data.Text.Text)
tokenCookieToMTokenTuple tc = case (Data.Text.splitOn "-" tc) of
  [id, token] -> Just (id, token)
  _ -> Nothing

reqToMStringTokenTuple :: Request -> Maybe (Data.Text.Text, Data.Text.Text)
reqToMStringTokenTuple = (tokenCookieToMTokenTuple =<<) . cookiesToMTokenCookie . reqToCookies

maybeRead :: Read a => String -> Maybe a
maybeRead = (liftM fst) . listToMaybe . reads

stringTokenTupleToMTokenTuple :: (Data.Text.Text, Data.Text.Text) -> Maybe (MemberId, String)
stringTokenTupleToMTokenTuple (a, b) = do
  id <- maybeRead $ Data.Text.unpack a
  token <- return $ Data.Text.unpack b
  return (id, token)

reqToMTokenTuple :: Request -> Maybe (MemberId, String)
reqToMTokenTuple = (stringTokenTupleToMTokenTuple =<<) . reqToMStringTokenTuple

_lPad0 :: Int -> String -> String
_lPad0 k s
  | length s < k = "0" ++ s
  | otherwise    = s

_generateCode8 :: IO String
_generateCode8 = do
  num <- randomRIO (0, 4294967295) :: IO Integer -- that's 16**8 - 1
  return $ _lPad0 8 $ showHex num ""

generateCode32 :: IO Code
generateCode32 = do
  arr <- replicateM 4 _generateCode8
  return $ concat arr

makeLoginCode :: Connection -> MemberId -> IO (Maybe LoginCode)
makeLoginCode conn idMember = do
  code <- generateCode32
  curTime <- getCurrentTime
  expiration <- return $ addUTCTime (60 * 60 * 24) curTime
  insertLoginCode conn (LoginCode undefined code (Finite expiration) idMember)
