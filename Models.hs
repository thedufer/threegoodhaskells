module Models where

import Database.PostgreSQL.Simple.Time

type Id = Int

type MemberId = Id
type Email = String
type SendTime = Int
data Member = Member MemberId Email Bool SendTime UTCTimestamp deriving (Show)

type LoginCodeId = Id
type Code = String
data LoginCode = LoginCode LoginCodeId Code UTCTimestamp MemberId deriving (Show)

type TokenId = Id
data Token = Token TokenId Code MemberId deriving (Show)

type PostId = Id
type PostToken = String
data Post = Post PostId (Maybe String) UTCTimestamp PostToken MemberId deriving (Show)

type AttachmentId = Id
type Url = String
type AttachmentType = String
data Attachment = Attachment AttachmentId Url AttachmentType PostId

type AnnouncementId = Id
data Announcement = Announcement AnnouncementId Bool String deriving (Show)

memberToId :: Member -> MemberId
memberToId (Member id _ _ _ _) = id

memberToEmail :: Member -> Email
memberToEmail (Member _ email _ _ _) = email

loginCodeToMemberId :: LoginCode -> MemberId
loginCodeToMemberId (LoginCode _ _ _ id) = id

loginCodeToCode :: LoginCode -> Code
loginCodeToCode (LoginCode _ code _ _) = code

tokenToMemberId :: Token -> MemberId
tokenToMemberId (Token _ _ id) = id

postToMemberId :: Post -> MemberId
postToMemberId (Post _ _ _ _ id) = id

postToMString :: Post -> Maybe String
postToMString (Post _ mString _ _ _) = mString

announcementToString :: Announcement -> String
announcementToString (Announcement _ _ str) = str
