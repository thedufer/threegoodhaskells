module Auth where

import Models
import DB
import qualified Data.Text
import Control.Monad (liftM)
import Web.Cookie (CookiesText, parseCookiesText)
import Database.PostgreSQL.Simple
import Network.Wai
import Data.List (find)

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

stringTokenTupleToTokenTuple :: (Data.Text.Text, Data.Text.Text) -> (MemberId, String)
stringTokenTupleToTokenTuple (a, b) = (read $ Data.Text.unpack a, Data.Text.unpack b)

reqToMTokenTuple :: Request -> Maybe (MemberId, String)
reqToMTokenTuple = (liftM stringTokenTupleToTokenTuple) . reqToMStringTokenTuple
