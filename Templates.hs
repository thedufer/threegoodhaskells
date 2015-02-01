module Templates (home, login) where

import Lucid
import Data.Text
import Data.String
import qualified Data.Text.Lazy as TL
import Models

login :: Maybe String -> TL.Text
login mStr = renderText $ html_ $ do
  Templates.head
  body_ $ do
    div_ [class_ "container"] $ do
      Templates.header [loginLink True]
      h3_ "Log In"
      form_ [action_ "login", method_ "post"] $ do
        div_ [style_ "display: inline-block;"] $ do
          input_ [type_ "email", placeholder_ "Email Address", name_ "email", class_ "form-control"]
        input_ [type_ "submit", value_ "Log In", class_ "btn btn-success"]

loginLink :: Bool -> Html ()
loginLink = headerLink "login" "Log In"

headerLink :: Text -> Html () -> Bool -> Html ()
headerLink url text True  = li_ [class_ "active"] $ do
  a_ [href_ url] text
headerLink url text False = li_ $ do
  a_ [href_ url] text

header :: [Html ()] -> Html ()
header lis = div_ [class_ "header"] $ do
  ul_ [class_ "nav nav-pills pull-right"] $ do
    sequence_ lis
  h3_ [class_ "text-muted"] "Three Good Things"

head :: Html ()
head = head_ $ do
  script_ [src_ "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"] ("" :: Text)
  link_ [rel_ "stylesheet", href_ "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css"]
  link_ [rel_ "stylesheet", href_ "http://maxcdn.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap-theme.min.css"]
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

home :: Maybe Member -> TL.Text
home u = renderText $ html_ $ do
  Templates.head
  body_ $ do
    div_ [class_ "container"] $ do
      Templates.header [loginLink False]
      div_ [class_ "jumbotron"] $ do
        h2_ [style_ "margin-top: -18px;"] "Reflect on the good things"
        br_ []
        h3_ "It's simple!"
        br_ []
        p_ [class_ "col-xs-4"] "1. We email you every day"
        p_ [class_ "col-xs-4"] "2. Reply with three good things about your day"
        p_ [class_ "col-xs-4"] "3. You get a collection of daily reflections"
        p_ [class_ "col-xs-12", style_ "padding-top: 12px;"] "All we need is your email address:"
        form_ [action_ "signup", method_ "post"] $ do
          div_ [style_ "display: inline-block;"] $ do
            input_ [type_ "email", placeholder_ "Email Address", name_ "email", class_ "form-control"]
          input_ [type_ "submit", value_ "Sign Up", class_ "btn btn-success"]
