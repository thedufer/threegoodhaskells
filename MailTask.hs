module MailTask (taskForever) where

import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay)

import qualified DB.Member
import qualified DB.Post
import Models
import qualified Time
import qualified Mail
import qualified Auth

taskForever :: Connection -> IO ()
taskForever conn = void $ forever $ do
  task conn
  threadDelay (10 * 1000 * 1000)

task :: Connection -> IO ()
task conn = do
  members <- DB.Member.membersNeedEmail conn
  mapM (taskForMember conn) members
  putStrLn $ show $ length members
  return ()

taskForMember :: Connection -> Member -> IO ()
taskForMember conn member = do
  mailMember conn member
  DB.Member.bumpNextEmailDate conn member
  return ()

mailMember :: Connection -> Member -> IO Bool
mailMember conn member = do
  mPost <- DB.Post.newPost conn member
  case mPost of
    Just (Post idPost _ (Finite date) postToken _) -> do
      mToken <- Auth.makeToken conn (memberToId member)
      case mToken of
        Just token -> do
          Mail.sendOtherPostMail conn member idPost postToken (Time.formatPostDate date)
        _ -> return False
    _ -> return False
