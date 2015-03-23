module MailTask (taskForever, getMPrevPostTuple) where

import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Control.Monad (void, forever, liftM, msum)
import Control.Monad.Reader (liftIO)
import Control.Concurrent (threadDelay)
import Data.Time.Clock (UTCTime)
import Data.Time.Lens (modL, day, month, year)

import DB
import qualified DB.Member
import qualified DB.Post
import Models
import qualified Time
import qualified Mail
import qualified Auth

prevPostChoices :: [(String, UTCTime -> UTCTime)]
prevPostChoices = [
    ("One week ago", modL day (subtract 7))
  , ("Two weeks ago", modL day (subtract 14))
  , ("Three weeks ago", modL day (subtract 21))
  , ("One month ago", modL month (subtract 1))
  , ("Two months ago", modL month (subtract 2))
  , ("Three months ago", modL month (subtract 3))
  , ("Six months ago", modL month (subtract 6))
  , ("Nine months ago", modL month (subtract 9))
  , ("One year ago", modL year (subtract 1))
  ]

prevPostChoiceToMPrevPostTuple :: MemberId -> (String, UTCTime -> UTCTime) -> DatabaseM (Maybe (String, String))
prevPostChoiceToMPrevPostTuple idMember (sTime, modTime) = do
  curDate <- liftIO Time.currentPostDate
  let searchDate = modTime curDate
  mPost <- DB.Post.dateToPost idMember searchDate
  let mString = mPost >>= postToMString
      fullSTime = sTime ++ " on " ++ Time.formatPostDate searchDate
  return $ liftM (\x -> (fullSTime, x)) mString

getMPrevPostTuple :: MemberId -> DatabaseM (Maybe (String, String))
getMPrevPostTuple idMember = liftM msum $ mapM (prevPostChoiceToMPrevPostTuple idMember) (reverse prevPostChoices)

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
        Just token -> do
          mPrevPostTuple <- getMPrevPostTuple (memberToId member)
          Mail.sendOtherPostMail member idPost postToken (Time.formatPostDate date) mPrevPostTuple
        _ -> return False
    _ -> return False
