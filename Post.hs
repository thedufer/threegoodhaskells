module Post (addToPost) where

import Database.PostgreSQL.Simple (Connection)
import Web.Scotty (File)
import qualified Settings
import Data.Attoparsec.Text
import qualified Data.Text
import Data.Maybe (isJust)

import Models
import qualified Time
import qualified DB.Post

parseEmail :: Data.Text.Text -> Either String (MemberId, PostToken)
parseEmail = parseOnly $ do
  string "post+"
  idMember <- decimal
  string "+"
  token <- count 32 (satisfy (inClass "0123456789abcdef"))
  string "@"
  string (Data.Text.pack Settings.domain)
  return (idMember, token)

addToPost :: Connection -> String -> String -> [File] -> IO Bool
addToPost conn email text fs = do
  let eParsedEmail = parseEmail (Data.Text.pack email)
  case eParsedEmail of
    Left _ -> return False
    Right (_, postToken) -> do
      mPost <- DB.Post.addToPost conn postToken text
      return (isJust mPost)
