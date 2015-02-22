import Web.Scotty
import Network.Wai
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as C8

import qualified Templates.Pages as Pages
import qualified Auth
import qualified Models
import qualified DB.Member
import qualified Mail

import Data.List (find)
import qualified Data.Text.Lazy as L
import Control.Monad (liftM)

showToHtml :: Show a => a -> ActionM ()
showToHtml = html . L.pack . show

getParam :: L.Text -> [Param] -> Maybe L.Text
getParam t = (liftM snd) . find (\x -> (fst x) == t)

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='threegoodhaskells'"
  scotty 3000 $ do
    get "/" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      html $ Pages.home mMember
    get "/login" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      case mMember of
        Nothing -> html $ Pages.login Nothing
        Just member -> redirect "/"
    get "/signup" $ do
      req <- request
      ps <- params
      html $ Pages.signup $ (liftM L.unpack) $ getParam "err" ps
    post "/signup" $ do
      req <- request
      ps <- params
      case (getParam "email" ps) of
        Nothing -> redirect "/"
        Just email -> do
          mMember <- liftIO $ DB.Member.newMember conn (L.unpack email)
          case mMember of
            Nothing -> redirect "/signup?err=inuse"
            Just member -> do
              Mail.sendFirstPostMail conn member idPost token day
              redirect "/"
