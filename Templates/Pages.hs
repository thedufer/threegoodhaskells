module Templates.Pages (landing, login, signup, posts, settings, checkEmail) where

import Lucid
import Data.Text
import Data.String
import qualified Data.Text.Lazy as TL
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Control.Monad (void)

import Models
import Time (formatPostDate)

signupErrMap :: Maybe String -> Html ()
signupErrMap (Just "inuse") = p_ [class_ "text-danger"] "That email address is already in use."
signupErrMap (Just "unknown") = p_ [class_ "text-danger"] "Something went terribly wrong.  Try again or shoot me an email."
signupErrMap _ = ""

loginErrMap :: Maybe String -> Html ()
loginErrMap (Just "notfound") = p_ [class_ "text-danger"] "Email address not found."
loginErrMap (Just "badcode") = p_ [class_ "text-danger"] "Login code not recognized.  Please try again."
loginErrMap (Just "unknown") = p_ [class_ "text-danger"] "Unknown error.  Please try again later."
loginErrMap _ = ""

signup :: Maybe String -> TL.Text
signup mStr = renderText $ html_ $ do
  Templates.Pages.head
  body_ $
    div_ [class_ "container"] $ do
      Templates.Pages.header [loginLink False]
      h3_ "Sign Up"
      signupErrMap mStr
      signupForm

login :: Maybe String -> TL.Text
login mStr = renderText $ html_ $ do
  Templates.Pages.head
  body_ $
    div_ [class_ "container"] $ do
      Templates.Pages.header [loginLink True]
      h3_ "Log In"
      loginErrMap mStr
      form_ [action_ "login", method_ "post"] $ do
        div_ [style_ "display: inline-block;"] $
          input_ [type_ "email", placeholder_ "Email Address", name_ "email", class_ "form-control"]
        " "
        input_ [type_ "submit", value_ "Log In", class_ "btn btn-success"]

loginLink :: Bool -> Html ()
loginLink = headerLink "/login" "Log In"

postsLink :: Bool -> Html ()
postsLink = headerLink "/posts" "Posts"

settingsLink :: Bool -> Html ()
settingsLink = headerLink "/settings" "Settings"

logoutLink :: Html ()
logoutLink = headerLink "/logout" "Log Out" False

headerLink :: Text -> Html () -> Bool -> Html ()
headerLink url text True  = li_ [class_ "active"] $
  a_ [href_ url] text
headerLink url text False = li_ $
  a_ [href_ url] text

header :: [Html ()] -> Html ()
header lis = div_ [class_ "header"] $ do
  ul_ [class_ "nav nav-pills pull-right"] $
    sequence_ lis
  h3_ [class_ "text-muted"] "Three Good Things"

headPlus :: Html () -> Html ()
headPlus extra = head_ $ do
  script_ [src_ "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"] ("" :: Text)
  link_ [rel_ "stylesheet", href_ "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"]
  link_ [rel_ "stylesheet", href_ "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"]
  script_ [src_ "//maxcdn.bootstrapcdn.com/bootstrap/3.2.0/js/bootstrap.min.js"] ("" :: Text)
  style_ (Data.Text.unlines [
    ".container {",
    "  max-width: 730px;",
    "}",
    ".header h3 {",
    "  line-height: 40px;",
    "}",
    ".jumbotron {",
    "  text-align: center;",
    "}",
    ".header {",
    "  border-bottom: 1px solid #e5e5e5;",
    "}" ])
  extra

head :: Html ()
head = headPlus $ void ""

signupForm :: Html ()
signupForm = form_ [action_ "signup", method_ "post"] $ do
  div_ [style_ "display: inline-block;"] $
    input_ [type_ "email", placeholder_ "Email Address", name_ "email", class_ "form-control"]
  " "
  input_ [type_ "submit", value_ "Sign Up", class_ "btn btn-success"]

landing :: TL.Text
landing = renderText $ html_ $ do
  Templates.Pages.head
  body_ $
    div_ [class_ "container"] $ do
      Templates.Pages.header [loginLink False]
      div_ [class_ "jumbotron"] $ do
        h2_ [style_ "margin-top: -18px;"] "Reflect on the good things"
        br_ []
        h3_ "It's simple!"
        br_ []
        p_ [class_ "col-xs-4"] "1. We email you every day"
        p_ [class_ "col-xs-4"] "2. Reply with three good things about your day"
        p_ [class_ "col-xs-4"] "3. You get a collection of daily reflections"
        p_ [class_ "col-xs-12", style_ "padding-top: 12px;"] "All we need is your email address:"
        signupForm

postToHtml :: Post -> Html ()
postToHtml (Post _ (Just text) (Finite timestamp) _ _) = do
  h4_ $ toHtml $ formatPostDate timestamp
  mapM_ (p_ . toHtml) (Data.String.lines text)
postToHtml _ = void ""

posts :: [Post] -> TL.Text
posts ps = renderText $ html_ $ do
  Templates.Pages.head
  body_ $
    div_ [class_ "container"] $ do
      Templates.Pages.header [postsLink True, settingsLink False, logoutLink]
      h3_ "Posts"
      mapM_ postToHtml ps

settings :: Member -> TL.Text
settings (Member _ _ unsubscribed sendTime _) = renderText $ html_ $ do
  Templates.Pages.headPlus $
    script_ $ Data.Text.pack (Data.String.unlines [
      "var sendTimeUTC = " ++ show sendTime ++ ";",
      "window.onload = function() {",
      "  var d = new Date();",
      "  d.setUTCMinutes(sendTimeUTC % 60);",
      "  d.setUTCHours(Math.floor(sendTimeUTC / 60));",
      "  var hours = ((d.getHours() + 11) % 12) + 1;",
      "  var minutes = d.getMinutes().toString();",
      "  if(minutes.length == 1)",
      "    minutes = '0' + minutes;",
      "  var hemi = d.getHours() < 12 ? 'AM' : 'PM';",
      "  var form = document.getElementById('timeForm');",
      "  form.getElementsByClassName('hours')[0].value = hours;",
      "  form.getElementsByClassName('minutes')[0].value = minutes;",
      "  form.getElementsByClassName('hemi')[0].value = hemi;",
      "}",
      "var reportError = function() {",
      "  document.getElementById('error').innerText = 'That doesn\\'t look like a time to me';",
      "  return false;",
      "};",
      "window.prepareTime = function() {",
      "  var form = document.getElementById('timeForm');",
      "  var d = new Date();",
      "  var minutes = parseInt(form.getElementsByClassName('minutes')[0].value, 10);",
      "  var hours = parseInt(form.getElementsByClassName('hours')[0].value, 10);",
      "  var hemi = form.getElementsByClassName('hemi')[0].value.toUpperCase();",
      "  if(minutes > 59 || minutes < 0)",
      "    return reportError();",
      "  if(hours > 12 || hours < 1)",
      "    return reportError();",
      "  if(hemi !== 'AM' && hemi !== 'PM')",
      "    return reportError();",
      "  d.setMinutes(minutes);",
      "  d.setHours(((hours % 12) + (hemi === 'PM' ? 12 : 0)) % 24);",
      "  var newSendTimeUTC = d.getUTCHours() * 60 + d.getUTCMinutes();",
      "  form.getElementsByClassName('sendTime')[0].value = newSendTimeUTC;",
      "  return true;",
      "};" ])
  body_ $
    div_ [class_ "container"] $ do
      Templates.Pages.header [postsLink False, settingsLink True, logoutLink]
      p_ $ do
        h3_ "Settings"
        h4_ "Current email time"
        div_ [id_ "error", style_ "color: red;"] ""
        form_ [action_ "time", method_ "post", onsubmit_ "return prepareTime();", id_ "timeForm"] $ do
          p_ $ do
            input_ [type_ "text", class_ "hours", maxlength_ "2", size_ "2"]
            ":"
            input_ [type_ "text", class_ "minutes", maxlength_ "2", size_ "2"]
            " "
            input_ [type_ "text", class_ "hemi", maxlength_ "2", size_ "2"]
          input_ [type_ "hidden", class_ "sendTime", name_ "sendTime", value_ ""]
          button_ [type_ "submit", class_ "btn"] "Save"
        if unsubscribed
          then
            div_ $ do
              h4_ "Miss us?"
              form_ [action_ "subscribe", method_ "post"] $
                button_ [type_ "submit", class_ "btn"] "Resubscribe"
          else
            div_ $ do
              h4_ "Is the pressure overwhelming?"
              form_ [action_ "unsubscribe", method_ "post"] $
                button_ [type_ "submit", class_ "btn"] "Unsubscribe"

checkEmail :: TL.Text
checkEmail = renderText $ html_ $ do
  Templates.Pages.head
  body_ $
    div_ [class_ "container"] $ do
      Templates.Pages.header [loginLink False]
      h4_ "Check your email for a link!"
