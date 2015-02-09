module Mail () where

import qualified Rackspace.MailGun as MG
import qualified Data.ByteString.Lazy.Char8 as LBS
import Network.HTTP.Conduit (Response)

import Models
import Templates

sendMessage :: MG.Message -> IO (Response LBS.ByteString)
sendMessage = MG.sendMessage "mg.aarondufour.com" "key-992d49a8db2fb111f82303ba962f3559"

sendFirstPostMail :: Member -> PostId -> String -> Email -> String -> IO (Response LBS.ByteString)
sendFirstPostMail member postId token to day =
