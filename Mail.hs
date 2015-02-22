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

sendMessage :: MG.Message -> IO (Response LBS.ByteString)
sendMessage = MG.sendMessage "mg.aarondufour.com" "key-992d49a8db2fb111f82303ba962f3559"

sendFirstPostMail :: Connection -> Member -> PostId -> String -> String -> IO (Response LBS.ByteString)
sendFirstPostMail conn member idPost token day = do
  mLoginCode <- Auth.makeLoginCode conn (memberToId member)
  case mLoginCode of
    Nothing -> fail "Failed to build a login code"
    Just loginCode ->
      let from = Text.pack $ "Three Good Things <post+" ++ (show idPost) ++ "+" ++ token ++ "@threegoodthings.xyz>"
          to = memberToEmail member
          cc = Nothing
          bcc = Nothing
          subject = Just $ Text.pack $ "It's " ++ day ++ " - What are your 3 Good Things?"
          html = TM.firstPost (memberToId member) (loginCodeToCode loginCode)
      in sendMessage $ MG.HtmlMessage from (Text.pack to) cc bcc subject (TL.toStrict html)
