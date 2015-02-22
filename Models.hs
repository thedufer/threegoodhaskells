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
data Token = Token TokenId String MemberId deriving (Show)

type PostId = Id
data Post = Post PostId String UTCTimestamp String MemberId deriving (Show)

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
