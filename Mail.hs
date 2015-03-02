module Mail (sendFirstPostMail) where

import qualified Rackspace.MailGun as MG
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TL
import Network.HTTP.Conduit (Response)
import Database.PostgreSQL.Simple (Connection)

import Models
import qualified Templates.Mail as TM
import qualified Auth
import qualified Settings

sendMessage :: MG.Message -> IO (Response LBS.ByteString)
sendMessage = MG.sendMessage Settings.domain Settings.mailgunKey

makeFromEmail :: PostId -> PostToken -> Text.Text
makeFromEmail idPost token = Text.pack $ "Three Good Things <post+" ++ (show idPost) ++ "+" ++ token ++ "@" ++ Settings.domain ++ ">"

sendFirstPostMail :: Connection -> Member -> PostId -> PostToken -> String -> IO (Response LBS.ByteString)
sendFirstPostMail conn member idPost token day = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> fail "Failed to build a login code"
    Just loginCode ->
      let from = makeFromEmail idPost token
          to = Text.pack $ memberToEmail member
          cc = Nothing
          bcc = Nothing
          subject = Just $ Text.pack $ "It's " ++ day ++ " - What are your 3 Good Things?"
          html = TL.toStrict $ TM.firstPost (memberToId member) (loginCodeToCode loginCode)
      in sendMessage $ MG.HtmlMessage from to cc bcc subject html

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
