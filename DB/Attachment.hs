module DB.Attachment (insertAttachment, postToAttachments) where

import Models
import DB

import Database.PostgreSQL.Simple (Only(..))
import Control.Monad (liftM)
import Data.Maybe (listToMaybe)

rowToAttachment :: (AttachmentId, Url, AttachmentType, PostId) -> Attachment
rowToAttachment (id, url, typ, idPost) = Attachment id url typ idPost

rowsToMAttachment :: [(AttachmentId, Url, AttachmentType, PostId)] -> Maybe Attachment
rowsToMAttachment = liftM rowToAttachment . listToMaybe

rowsToAttachments :: [(AttachmentId, Url, AttachmentType, PostId)] -> [Attachment]
rowsToAttachments = map rowToAttachment

insertAttachment :: Attachment -> DatabaseM (Maybe Attachment)
insertAttachment (Attachment _ url typ idPost) =
  liftM rowsToMAttachment $ query "INSERT INTO \"Attachments\" (url, type, \"PostId\") VALUES (?, ?, ?) RETURNING id, url, type, \"PostId\"" (url, typ, idPost)

postToAttachments :: Post -> DatabaseM [Attachment]
postToAttachments (Post idPost _ _ _ _) =
  liftM rowsToAttachments $ query "SELECT id, url, type, \"PostId\" FROM \"Attachments\" WHERE \"PostId\" = ?" (Only idPost)
