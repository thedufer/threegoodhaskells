module MailTask (taskForever) where

import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Control.Monad (void, forever)
import Control.Monad.Reader (liftIO)
import Control.Concurrent (threadDelay)

import DB
import qualified DB.Member
import qualified DB.Post
import Models
import qualified Time
import qualified Mail
import qualified Auth

taskForever :: DatabaseM ()
taskForever = void $ forever $ do
  task
  liftIO $ threadDelay (10 * 1000 * 1000)

task :: DatabaseM ()
task = do
  members <- DB.Member.membersNeedEmail
  liftIO $ print $ length members
  mapM_ taskForMember members
  return ()

taskForMember :: Member -> DatabaseM ()
taskForMember member = do
  mailMember member
  DB.Member.bumpNextEmailDate member
  return ()

mailMember :: Member -> DatabaseM Bool
mailMember member = do
  mPost <- DB.Post.newPost member
  case mPost of
    Just (Post idPost _ (Finite date) postToken _) -> do
      mToken <- Auth.makeToken (memberToId member)
      case mToken of
        Just token ->
          Mail.sendOtherPostMail member idPost postToken (Time.formatPostDate date)
        _ -> return False
    _ -> return False
