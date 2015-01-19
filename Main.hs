import Web.Scotty
import Lucid
import Data.Text

import Data.Monoid (mconcat)

homepage :: Html ()
homepage = script_ [src_ "//ajax.googleapis.com/ajax/libs/jquery/2.1.1/jquery.min.js"] ("" :: Text)

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html $ renderText homepage
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
