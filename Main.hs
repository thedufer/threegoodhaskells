import Web.Scotty
import Network.Wai
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Control.Monad.Trans (liftIO)
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Web.Scotty.Cookie (deleteCookie, setCookie)
import Network.HTTP.Types.Status (status400)
import Control.Concurrent (forkIO)

import qualified Templates.Pages as Pages
import qualified Auth
import qualified Models
import qualified DB.Member
import qualified DB.Post
import qualified DB.LoginCode
import qualified Mail
import qualified Time
import qualified Post
import qualified MailTask
import Util (maybeRead)

import Data.List (find)
import qualified Data.Text.Lazy as L
import Control.Monad (liftM)

getParam :: L.Text -> [Param] -> Maybe L.Text
getParam t = liftM snd . find (\x -> fst x == t)

getIntParam :: L.Text -> [Param] -> Maybe Int
getIntParam t ps = maybeRead =<< liftM L.unpack (getParam t ps)

setUnsubscribed :: Connection -> Bool -> ActionM ()
setUnsubscribed conn unsubscribe = do
  req <- request
  mMember <- liftIO $ Auth.loadSession conn req
  case mMember of 
    Nothing -> redirect "/settings?err=unknown"
    Just member -> do
      liftIO $ DB.Member.setUnsubscribed conn (Models.memberToId member) unsubscribe
      redirect "/settings"

doLogin :: Connection -> Models.MemberId -> Models.Code -> L.Text -> ActionM ()
doLogin conn idMember code redirURL = do
  mMember <- liftIO $ DB.Member.idToMMember conn idMember
  case mMember of
    Nothing -> redirect "/login?err=badcode"
    Just member -> do
      mLoginCode <- liftIO $ DB.LoginCode.codeToMLoginCode conn code idMember
      case mLoginCode of
        Nothing -> redirect "/login?err=badcode"
        Just _ -> do
          mToken <- liftIO $ Auth.makeToken conn (Models.memberToId member)
          case mToken of
            Just token -> do
              setCookie (Auth.tokenToCookie token)
              redirect redirURL
            _ -> redirect "/login?err=unknown"

main :: IO ()
main = do
  conn <- connectPostgreSQL "dbname='threegoodhaskells'"
  forkIO $ MailTask.taskForever conn
  scotty 3000 $ do
    get "/" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      case mMember of
        Nothing -> html Pages.landing
        Just _ -> redirect "/posts"
    get "/login" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      case mMember of
        Nothing -> do
          ps <- params
          html $ Pages.login $ liftM L.unpack $ getParam "err" ps
        Just member -> redirect "/"
    get "/checkemail" $
      html Pages.checkEmail
    get "/login-link" $ do
      req <- request
      ps <- params
      case (getIntParam "id" ps, getParam "code" ps) of
        (Just id, Just code) ->
          case getParam "redirect" ps of
            Nothing -> doLogin conn id (L.unpack code) "/"
            Just "" -> doLogin conn id (L.unpack code) "/"
            Just redirect -> doLogin conn id (L.unpack code) redirect
        _ -> redirect "/login?err=unknown"
    post "/login" $ do
      req <- request
      ps <- params
      case getParam "email" ps of
        Nothing -> redirect "/"
        Just email -> do
          mMember <- liftIO $ DB.Member.emailToMMember conn (L.unpack email)
          case mMember of
            Nothing -> redirect "/login?err=notfound"
            Just member -> do
              succ <- liftIO $ Mail.sendLoginMail conn member
              if succ
                then redirect "/checkemail"
                else redirect "/login?err=unknown"
    get "/logout" $ do
      deleteCookie "token"
      redirect "/"
    get "/posts" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      case mMember of
        Nothing -> redirect "/"
        Just member -> do
          posts <- liftIO $ DB.Post.memberToPosts conn member
          html $ Pages.posts posts
    get "/settings" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      case mMember of
        Nothing -> redirect "/"
        Just member -> html $ Pages.settings member
    post "/time" $ do
      req <- request
      ps <- params
      mMember <- liftIO $ Auth.loadSession conn req
      case (mMember, getIntParam "sendTime" ps) of
        (Just member, Just sendTime) -> do
          liftIO $ DB.Member.setSendTime conn (Models.memberToId member) sendTime
          redirect "/settings"
        _ -> redirect "/settings?err=unknown"
      redirect "/settings"
    post "/subscribe" $
      setUnsubscribed conn False
    post "/unsubscribe" $
      setUnsubscribed conn True
    get "/signup" $ do
      req <- request
      mMember <- liftIO $ Auth.loadSession conn req
      case mMember of
        Just _ -> redirect "/"
        Nothing -> do
          ps <- params
          html $ Pages.signup $ liftM L.unpack $ getParam "err" ps
    post "/signup" $ do
      req <- request
      ps <- params
      case getParam "email" ps of
        Nothing -> redirect "/"
        Just email -> do
          mMember <- liftIO $ DB.Member.newMember conn (L.unpack email)
          case mMember of
            Nothing -> redirect "/signup?err=inuse"
            Just member -> do
              mPost <- liftIO $ DB.Post.newPost conn member
              case mPost of
                Just (Models.Post idPost _ (Finite date) postToken _) -> do
                  mToken <- liftIO $ Auth.makeToken conn (Models.memberToId member)
                  case mToken of
                    Just token -> do
                      liftIO $ Mail.sendFirstPostMail conn member idPost postToken (Time.formatPostDate date)
                      setCookie (Auth.tokenToCookie token)
                      redirect "/"
                    _ -> redirect "/signup?err=unknown"
                _ -> redirect "/signup?err=unknown"
    post "/message" $ do
      req <- request
      ps <- params
      fs <- files
      liftIO $ putStrLn "-------------------------"
      liftIO $ print ps
      liftIO $ putStrLn ""
      liftIO $ print fs
      liftIO $ putStrLn ""
      let
        mEmail = getParam "recipient" ps
        mText = getParam "stripped-text" ps
        mSubject = getParam "subject" ps
      case (mEmail, mText, mSubject) of
        (Just email, Just text, Just subject) -> do
          liftIO $ putStrLn $ show email ++ " " ++ show text ++ " " ++ show subject
          success <- liftIO $ Post.addToPost conn (L.unpack email) (L.unpack text) fs
          if success
            then html ""
            else status status400
        _ -> status status400
