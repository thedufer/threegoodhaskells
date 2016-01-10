module Mail (sendFirstPostMail, sendFirstPostResponseMail, sendOtherPostMail, sendLoginMail) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy as TL
import Control.Monad.Reader (liftIO)
import Network.HTTP.Conduit (Response)
import Network.Mail.Mime (Mail(..), Address(..), Alternatives(..), Part(..), Encoding(..))
import Data.Maybe (fromJust)
import System.IO.Error (catchIOError)

import Models
import DB
import qualified DB.Announcement
import qualified DB.Member
import qualified Templates.Mail as TM
import qualified Auth
import qualified Settings
import qualified Mailgun as MG

sendMessage :: String -> Mail -> DatabaseM Bool
sendMessage to mail = liftIO $ catchIOError
  (MG.sendMessage Settings.domain Settings.mailgunKey to mail >> return True)
  (\x -> return False)

makeFromEmail :: PostId -> PostToken -> Address
makeFromEmail idPost postToken = Address (Just "Three Good Things") (Text.pack ("post+" ++ show idPost ++ "+" ++ postToken ++ "@" ++ Settings.domain))

doNotReplyEmail :: Address
doNotReplyEmail = Address (Just "Three Good Things") (Text.pack ("do-not-reply@" ++ Settings.domain))

addressToString :: Address -> String
addressToString address = Text.unpack (fromJust $ addressName address) ++ " <" ++ Text.unpack (addressEmail address) ++ ">"

sendMail :: Email -> String -> Maybe String -> Text.Text -> DatabaseM Bool
sendMail to subject mReplyTo html =
  let from = doNotReplyEmail
      toAddress = Address Nothing (Text.pack to)
      cc = []
      bcc = []
      headers = case mReplyTo of
                  Nothing -> [("Subject", Text.pack subject)]
                  Just replyTo -> [("Reply-To", Text.pack replyTo), ("Subject", Text.pack subject)]
      parts = [Part "text/html" QuotedPrintableText Nothing [] (LTE.encodeUtf8 $ TL.fromStrict html)]
  in sendMessage to $ Mail from [toAddress] cc bcc headers [parts]

sendFirstPostMail :: Member -> PostId -> PostToken -> String -> DatabaseM Bool
sendFirstPostMail member idPost postToken day = do
  mLoginCode <- Auth.makeLoginCode (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      sendMail
        (memberToEmail member) 
        ("It's " ++ day ++ " - What are your 3 Good Things?")
        (Just $ addressToString $ makeFromEmail idPost postToken)
        (TL.toStrict $ TM.firstPost (memberToId member) (loginCodeToCode loginCode))

sendFirstPostResponseMail :: Member -> PostId -> PostToken -> String -> DatabaseM Bool
sendFirstPostResponseMail member idPost postToken oldSubject = do
  mLoginCode <- Auth.makeLoginCode (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      sendMail
        (memberToEmail member)
        ("Re: " ++ oldSubject)
        (Just $ addressToString $ makeFromEmail idPost postToken)
        (TL.toStrict $ TM.firstPostResponse (memberToId member) (loginCodeToCode loginCode))

sendOtherPostMail :: Member -> PostId -> PostToken -> String -> Maybe (String, String, [Attachment]) -> DatabaseM Bool
sendOtherPostMail member idPost postToken day mPrevPostTuple = do
  mLoginCode <- Auth.makeLoginCode (memberToId member)
  mAnnouncement <- DB.Announcement.getAnnouncementForMember member
  DB.Member.setGotActiveAnnouncement member
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      sendMail
        (memberToEmail member)
        ("It's " ++ day ++ " - What are your 3 Good Things?")
        (Just $ addressToString $ makeFromEmail idPost postToken)
        (TL.toStrict $ TM.otherPost (memberToId member) (loginCodeToCode loginCode) mPrevPostTuple mAnnouncement)

sendLoginMail :: Member -> DatabaseM Bool
sendLoginMail member = do
  mLoginCode <- Auth.makeLoginCode (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      sendMail
        (memberToEmail member)
        "Three Good Things Login Link"
        Nothing
        (TL.toStrict $ TM.login (memberToId member) (loginCodeToCode loginCode))
