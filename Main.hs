import Web.Scotty.Trans
import Network.Wai
import Database.PostgreSQL.Simple (connectPostgreSQL, Connection)
import Control.Monad (liftM)
import Control.Monad.Trans (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (mapStateT)
import Control.Monad.Trans.Either
import qualified Data.ByteString.Char8 as C8
import Data.List (find)
import qualified Data.Text.Lazy as L
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Web.Scotty.Cookie (deleteCookie, setCookie)
import Network.HTTP.Types.Status (status400)
import Control.Concurrent (forkIO)
import Data.Functor ((<$>))

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
import qualified Settings
import Util (maybeRead)
import DB

type M a = ActionT L.Text DatabaseM a

getParam :: L.Text -> [Param] -> Maybe L.Text
getParam t = liftM snd . find (\x -> fst x == t)

getIntParam :: L.Text -> [Param] -> Maybe Int
getIntParam t ps = maybeRead =<< liftM L.unpack (getParam t ps)

liftDB :: MonadIO m => Connection -> DatabaseM a -> m a
liftDB conn inner = liftIO $ runDB inner conn

setUnsubscribed :: Bool -> M ()
setUnsubscribed unsubscribe = do
  req <- request
  mMember <- lift $ Auth.loadSession req
  case mMember of 
    Nothing -> redirect "/settings?err=unknown"
    Just member -> do
      lift $ DB.Member.setUnsubscribed (Models.memberToId member) unsubscribe
      redirect "/settings"

doLogin :: Models.MemberId -> Models.Code -> L.Text -> M ()
doLogin idMember code redirURL = do
  mMember <- lift $ DB.Member.idToMMember idMember
  case mMember of
    Nothing -> redirect "/login?err=badcode"
    Just member -> do
      mLoginCode <- lift $ DB.LoginCode.codeToMLoginCode code idMember
      case mLoginCode of
        Nothing -> redirect "/login?err=badcode"
        Just _ -> do
          mToken <- lift $ Auth.makeToken (Models.memberToId member)
          case mToken of
            Just token -> do
              setCookie (Auth.tokenToCookie token)
              redirect redirURL
            _ -> redirect "/login?err=unknown"

home :: M ()
home = do
  req <- request
  mMember <- lift $ Auth.loadSession req
  maybe (html Pages.landing)
        (const $ redirect "/posts")
        mMember

doBoth :: Monad m => (a -> m b) -> EitherT a m a -> m b
doBoth f = eitherT f f

fromMaybe :: Monad m => Maybe a -> e -> EitherT e m a
fromMaybe Nothing def = left def
fromMaybe (Just a) _ = right a

doSignup :: M ()
doSignup = doBoth redirect $ do
  let liftDB = lift . lift
  ps <- lift params
  email <- fromMaybe (getParam "email" ps) "/"

  mMember <- liftDB $ DB.Member.newMember (L.unpack email)
  member <- maybe (left "/signup?err=inuse") right mMember

  mPost <- liftDB $ DB.Post.newPost member
  (Models.Post idPost _ (Finite date) postToken _) <-
    maybe (left "/signup?err=unknown") right mPost

  mToken <- liftDB $ Auth.makeToken (Models.memberToId member)
  token <- maybe (left "/signup?err=unknown") right mToken

  liftDB $ Mail.sendFirstPostMail member idPost postToken (Time.formatPostDate date)
  lift $ setCookie (Auth.tokenToCookie token)
  return "/settings"

main :: IO ()
main = do
  conn <- connectPostgreSQL Settings.dbConnectionString 
  forkIO $ liftDB conn MailTask.taskForever
  scottyT Settings.port (liftDB conn) $ do
    get "/" home
    get "/login" $ do
      req <- request
      mMember <- lift $ Auth.loadSession req
      case mMember of
        Nothing -> do
          ps <- params
          html $ Pages.login $ liftM L.unpack $ getParam "err" ps
        Just member -> redirect "/"
    get "/checkemail" $
      html Pages.checkEmail
    get "/login-link" $ do
      ps <- params
      case (getIntParam "id" ps, getParam "code" ps) of
        (Just id, Just code) ->
          case getParam "redirect" ps of
            Nothing -> doLogin id (L.unpack code) "/"
            Just "" -> doLogin id (L.unpack code) "/"
            Just redirect -> doLogin id (L.unpack code) redirect
        _ -> redirect "/login?err=unknown"
    post "/login" $ do
      ps <- params
      case getParam "email" ps of
        Nothing -> redirect "/"
        Just email -> do
          mMember <- lift $ DB.Member.emailToMMember (L.unpack email)
          case mMember of
            Nothing -> redirect "/login?err=notfound"
            Just member -> do
              succ <- lift $ Mail.sendLoginMail member
              if succ
                then redirect "/checkemail"
                else redirect "/login?err=unknown"
    get "/logout" $ do
      deleteCookie "token"
      redirect "/"
    get "/posts" $ do
      req <- request
      mMember <- lift $ Auth.loadSession req
      case mMember of
        Nothing -> redirect "/"
        Just member -> do
          posts <- lift $ DB.Post.memberToPosts member
          html $ Pages.posts posts
    get "/settings" $ do
      req <- request
      mMember <- lift $ Auth.loadSession req
      case mMember of
        Nothing -> redirect "/"
        Just member -> html $ Pages.settings member
    post "/time" $ do
      req <- request
      ps <- params
      mMember <- lift $ Auth.loadSession req
      case (mMember, getIntParam "sendTime" ps) of
        (Just member, Just sendTime) -> do
          lift $ DB.Member.setSendTime (Models.memberToId member) sendTime
          redirect "/settings"
        _ -> redirect "/settings?err=unknown"
      redirect "/settings"
    post "/subscribe" $
      setUnsubscribed False
    post "/unsubscribe" $
      setUnsubscribed True
    get "/signup" $ do
      req <- request
      mMember <- lift $ Auth.loadSession req
      case mMember of
        Just _ -> redirect "/"
        Nothing -> do
          ps <- params
          html $ Pages.signup $ liftM L.unpack $ getParam "err" ps
    post "/signup" doSignup
    post "/message" $ do
      ps <- params
      fs <- files
      let
        mEmail = getParam "recipient" ps
        mText = getParam "stripped-text" ps
        mSubject = getParam "subject" ps
      case (mEmail, mText, mSubject) of
        (Just email, Just text, Just subject) -> do
          liftIO $ putStrLn $ show email ++ " " ++ show text ++ " " ++ show subject
          success <- lift $ Post.addToPost (L.unpack email) (L.unpack text) fs
          if success
            then html ""
            else status status400
        _ -> status status400
