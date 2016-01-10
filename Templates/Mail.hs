module Templates.Mail (firstPost, firstPostResponse, otherPost, login) where

import Lucid
import Data.Text
import Data.String
import qualified Data.Text.Lazy as TL

import qualified Settings
import Models
import Templates.Util (textToHtml, attachmentsToHtml)

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
  p_ "Just reply to this email with your Three Good Things, and we'll save it for you!"
  p_ "Or just write anything.  We'll hold on to whatever it is!"
  footer idMember code

firstPostResponse :: MemberId -> Code -> TL.Text
firstPostResponse idMember code = renderText $ do
  p_ "Congrats on your writing your first entry!"
  p_ $ do
    "You can check it out "
    a_ [href_ $ loginLink idMember code "/posts"] "here"
    "."
  footer idMember code

otherPost :: MemberId -> Code -> Maybe (String, String, [Attachment]) -> Maybe Announcement -> TL.Text
otherPost idMember code mPrevPostTuple mAnnouncement = renderText $ do
  case mAnnouncement of
    Just str -> do
      toHtmlRaw (announcementToString str)
      br_ []
      br_ []
    Nothing -> ""
  p_ "Just reply to this email with your Three Good Things."
  case mPrevPostTuple of
    Just (sDate, text, as) -> do
      br_ []
      br_ []
      br_ []
      p_ $ fromString $ sDate ++ " you wrote:"
      br_ []
      p_ $ textToHtml text
      attachmentsToHtml as
      br_ []
    Nothing -> ""
  footer idMember code

login :: MemberId -> Code -> TL.Text
login idMember code = renderText $ do
  p_ $ do
    a_ [href_ $ loginLink idMember code "/posts"] "Click here"
    " to log in."
  footer idMember code
