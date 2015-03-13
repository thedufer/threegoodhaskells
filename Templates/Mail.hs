module Templates.Mail (firstPost, firstPostResponse, otherPost, login) where

import Lucid
import Data.Text
import Data.String
import qualified Data.Text.Lazy as TL
import qualified Settings
import Models

loginLink :: MemberId -> Code -> String -> Text
loginLink idMember code redirect = pack $
  Settings.fullDomain ++ "/login-link?id=" ++ show idMember ++ "&code=" ++ code ++ "&redirect=" ++ redirect

footer :: MemberId -> Code -> Html ()
footer idMember code = do
  br_ []
  br_ []
  br_ []
  a_ [href_ $ loginLink idMember code "/posts"] "Previous Posts"
  " "
  a_ [href_ $ loginLink idMember code "/settings"] "Unsubscribe"

firstPost :: MemberId -> Code -> TL.Text
firstPost idMember code = renderText $ do
  "Just reply to this email with your Three Good Things, and we'll save it for you!"
  br_ []
  "Or just write anything.  We'll hold on to whatever it is!"
  footer idMember code

firstPostResponse :: MemberId -> Code -> TL.Text
firstPostResponse idMember code = renderText $ do
  "Congrats on your writing your first entry!"
  br_ []
  "You can check it out "
  a_ [href_ $ loginLink idMember code "/posts"] "here"
  "."
  footer idMember code

otherPost :: MemberId -> Code -> TL.Text
otherPost idMember code = renderText $ do
  "Just reply to this email with your Three Good Things."
  br_ []
  footer idMember code

login :: MemberId -> Code -> TL.Text
login idMember code = renderText $ do
  a_ [href_ $ loginLink idMember code "/posts"] "Click here"
  " to log in."
  footer idMember code
