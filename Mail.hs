module Mail (sendFirstPostMail) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Lazy as TL
import Network.HTTP.Conduit (Response)
import Database.PostgreSQL.Simple (Connection)
import Network.Mail.Mime (Mail(..), Address(..), Alternatives(..), Part(..), Encoding(..))
import Data.Maybe (fromJust)

import Models
import qualified Templates.Mail as TM
import qualified Auth
import qualified Settings
import qualified Mailgun as MG

sendMessage :: String -> Mail -> IO (Response LBS.ByteString)
sendMessage = MG.sendMessage Settings.domain Settings.mailgunKey

makeFromEmail :: PostId -> PostToken -> Address
makeFromEmail idPost token = Address (Just "Three Good Things") (Text.pack ("post+" ++ (show idPost) ++ "+" ++ token ++ "@" ++ Settings.domain))

doNotReplyEmail :: Address
doNotReplyEmail = Address (Just "Three Good Things") (Text.pack ("do-not-reply@" ++ Settings.domain))

sendFirstPostMail :: Connection -> Member -> PostId -> PostToken -> String -> IO (Response LBS.ByteString)
sendFirstPostMail conn member idPost token day = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> fail "Failed to build a login code"
    Just loginCode ->
      let from = doNotReplyEmail
          to = memberToEmail member
          toAddress = Address Nothing (Text.pack to)
          cc = []
          bcc = []
          subject = "It's " ++ day ++ " - What are your 3 Good Things?"
          replyToAddress = makeFromEmail idPost token
          replyTo = (Text.unpack $ fromJust $ addressName replyToAddress) ++ " <" ++ (Text.unpack $ addressEmail replyToAddress) ++ ">"
          headers = [("Reply-To", Text.pack replyTo), ("Subject", Text.pack subject)]
          html = TL.toStrict $ TM.firstPost (memberToId member) (loginCodeToCode loginCode)
          parts = [Part "text/html" QuotedPrintableText Nothing [] (LTE.encodeUtf8 $ TL.fromStrict html)]
      in sendMessage to $ Mail from [toAddress] cc bcc headers [parts]

{-
sendFirstPostResponseMail :: Connection -> Member -> PostId -> PostToken -> String -> IO (Response LBS.ByteString)
sendFirstPostResponseMail conn member idPost token oldSubject = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> fail "Failed to build a login code"
    Just loginCode ->
      let from = makeFromEmail idPost token
          to = Text.pack $ memberToEmail member
          cc = Nothing
          bcc = Nothing
          subject = Just $ Text.pack $ "Re: " ++ oldSubject
          html = TL.toStrict $ TM.firstPostResponse (memberToId member) (loginCodeToCode loginCode)
      in sendMessage $ MG.HtmlMessage from to cc bcc subject html
-}
