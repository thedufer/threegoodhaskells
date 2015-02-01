import Web.Scotty
import Network.Wai
import Database.PostgreSQL.Simple (connectPostgreSQL)
import Control.Monad.Trans (liftIO)

import qualified Templates
import qualified Auth
import qualified Models
import qualified DB

import qualified Data.Text.Lazy as L

showToHtml :: Show a => a -> ActionM ()
showToHtml = html . L.pack . show

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='threegoodhaskells'"
  scotty 3000 $ do
    get "/" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      html $ Templates.home mMember
    get "/login" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      html $ Templates.login Nothing
