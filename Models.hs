module Models where

type Email = String
data User = User Email deriving (Show)
userToEmail :: User -> Email
userToEmail (User email) = email
