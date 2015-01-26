module Templates (homepage) where

import Lucid
import Data.Text

-- homepage :: Data.Text.Internal.Lazy.Text
homepage = renderText $ html_ $ do
  head_ $ do
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
  body_ $ do
    div_ [class_ "container"] $ do
      div_ [class_ "header"] $ do
        ul_ [class_ "nav nav-pills pull-right"] $ do
          li_ $ do
            a_ [href_ "login"] "Log In"
        h3_ [class_ "text-muted"] "Three Good Things"
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
