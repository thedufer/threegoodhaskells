module Templates.Mail (firstPost, firstPostResponse) where

import Lucid
import Data.Text
import Data.String
import qualified Data.Text.Lazy as TL
import Models

footer :: MemberId -> Code -> Html ()
footer idMember code = do
  br_ []
  br_ []
  br_ []
  a_ [href_ $ pack ("https://threegoodthings.xyz/login-link?id=" ++ (show idMember) ++ "&code=" ++ code ++ "&redirect=/posts")] "Previous Posts"
  " "
  a_ [href_ $ pack ("https://threegoodthings.xyz/login-link?id=" ++ (show idMember) ++ "&code=" ++ code ++ "&redirect=/settings")] "Unsubscribe"

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
  a_ [href_ $ pack ("https://threegoodthings.xyz/login-link?id=" ++ (show idMember) ++ "&code=" ++ code ++ "&redirect=/posts")] "here"
  "."
  footer idMember code
