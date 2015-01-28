module Auth where

import Models
import qualified Data.Text
import Control.Monad (liftM)
import Web.Cookie (CookiesText, parseCookiesText)
import Network.Wai
import Data.List (find)

loadSession :: Request -> Maybe Member
loadSession req = Nothing

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

reqToMTokenTuple :: Request -> Maybe (Data.Text.Text, Data.Text.Text)
reqToMTokenTuple = (tokenCookieToMTokenTuple =<<) . cookiesToMTokenCookie . reqToCookies
