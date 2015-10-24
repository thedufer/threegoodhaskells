module Post (addToPost) where

import Web.Scotty (File)
import qualified Settings
import Data.Attoparsec.Text
import qualified Data.Text
import Data.Maybe (isJust, catMaybes)
import Control.Monad.Trans (liftIO)
import Network.Wai.Parse (fileContentType)
import qualified Data.ByteString.Char8 as BSC

import Models
import DB
import DB.Attachment (insertAttachment)
import Image
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

addToPost :: String -> String -> [File] -> DatabaseM Bool
addToPost email text fs = case parseEmail (Data.Text.pack email) of
  Left _ -> return False
  Right (_, postToken) -> do
    mPost <- DB.Post.addToPost postToken text
    case mPost of
      Nothing -> return False
      Just (Post idPost _ _ _ _) -> do
        images <- return $ filter isImage fs
        imageUrls <- liftIO $ mapM fileToUrl images
        attachments <- return $ map (_pairToAttachment idPost) $ catMaybes $ map _mSndToMPair $ zip images imageUrls
        mapM_ insertAttachment attachments
        return True

_mSndToMPair :: (a, Maybe b) -> Maybe (a, b)
_mSndToMPair (a, Just b) = Just (a, b)
_mSndToMPair (_, Nothing) = Nothing

_pairToAttachment :: PostId -> (File, Url) -> Attachment
_pairToAttachment idPost ((_, info), url) = Attachment undefined url (BSC.unpack $ fileContentType info) idPost
