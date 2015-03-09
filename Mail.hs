module Mail (sendFirstPostMail, sendFirstPostResponseMail, sendOtherPostMail, sendLoginMail) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy as TL
import Network.HTTP.Conduit (Response)
import Database.PostgreSQL.Simple (Connection)
import Network.Mail.Mime (Mail(..), Address(..), Alternatives(..), Part(..), Encoding(..))
import Data.Maybe (fromJust)
import System.IO.Error (catchIOError)

import Models
import qualified Templates.Mail as TM
import qualified Auth
import qualified Settings
import qualified Mailgun as MG

sendMessage :: String -> Mail -> IO Bool
sendMessage to mail = catchIOError
  ((MG.sendMessage Settings.domain Settings.mailgunKey to mail) >> (return True))
  (\x -> return False)

makeFromEmail :: PostId -> PostToken -> Address
makeFromEmail idPost postToken = Address (Just "Three Good Things") (Text.pack ("post+" ++ (show idPost) ++ "+" ++ postToken ++ "@" ++ Settings.domain))

doNotReplyEmail :: Address
doNotReplyEmail = Address (Just "Three Good Things") (Text.pack ("do-not-reply@" ++ Settings.domain))

addressToString :: Address -> String
addressToString address = (Text.unpack $ fromJust $ addressName address) ++ " <" ++ (Text.unpack $ addressEmail address) ++ ">"

sendFirstPostMail :: Connection -> Member -> PostId -> PostToken -> String -> IO Bool
sendFirstPostMail conn member idPost postToken day = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      let from = doNotReplyEmail
          to = memberToEmail member
          toAddress = Address Nothing (Text.pack to)
          cc = []
          bcc = []
          subject = "It's " ++ day ++ " - What are your 3 Good Things?"
          replyToAddress = makeFromEmail idPost postToken
          replyTo = addressToString replyToAddress
          headers = [("Reply-To", Text.pack replyTo), ("Subject", Text.pack subject)]
          html = TL.toStrict $ TM.firstPost (memberToId member) (loginCodeToCode loginCode)
          parts = [Part "text/html" QuotedPrintableText Nothing [] (LTE.encodeUtf8 $ TL.fromStrict html)]
      in sendMessage to $ Mail from [toAddress] cc bcc headers [parts]

sendFirstPostResponseMail :: Connection -> Member -> PostId -> PostToken -> String -> IO Bool
sendFirstPostResponseMail conn member idPost postToken oldSubject = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      let from = doNotReplyEmail
          to = memberToEmail member
          toAddress = Address Nothing (Text.pack to)
          cc = []
          bcc = []
          subject = "Re: " ++ oldSubject
          replyToAddress = makeFromEmail idPost postToken
          replyTo = addressToString replyToAddress
          headers = [("Reply-To", Text.pack replyTo), ("Subject", Text.pack subject)]
          html = TL.toStrict $ TM.firstPostResponse (memberToId member) (loginCodeToCode loginCode)
          parts = [Part "text/html" QuotedPrintableText Nothing [] (LTE.encodeUtf8 $ TL.fromStrict html)]
      in sendMessage to $ Mail from [toAddress] cc bcc headers [parts]

sendOtherPostMail :: Connection -> Member -> PostId -> PostToken -> String -> IO Bool
sendOtherPostMail conn member idPost postToken day = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      let from = doNotReplyEmail
          to = memberToEmail member
          toAddress = Address Nothing (Text.pack to)
          cc = []
          bcc = []
          subject = "It's " ++ day ++ " - What are your 3 Good Things?"
          replyToAddress = makeFromEmail idPost postToken
          replyTo = addressToString replyToAddress
          headers = [("Reply-To", Text.pack replyTo), ("Subject", Text.pack subject)]
          html = TL.toStrict $ TM.otherPost (memberToId member) (loginCodeToCode loginCode)
          parts = [Part "text/html" QuotedPrintableText Nothing [] (LTE.encodeUtf8 $ TL.fromStrict html)]
      in sendMessage to $ Mail from [toAddress] cc bcc headers [parts]

sendLoginMail :: Connection -> Member -> IO Bool
sendLoginMail conn member = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> return False
    Just loginCode ->
      let from = doNotReplyEmail
          to = memberToEmail member
          toAddress = Address Nothing (Text.pack to)
          cc = []
          bcc = []
          subject = "Three Good Things Login Link"
          headers = [("Subject", Text.pack subject)]
          html = TL.toStrict $ TM.login (memberToId member) (loginCodeToCode loginCode)
          parts = [Part "text/html" QuotedPrintableText Nothing [] (LTE.encodeUtf8 $ TL.fromStrict html)]
      in sendMessage to $ Mail from [toAddress] cc bcc headers [parts]
