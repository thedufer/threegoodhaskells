import Web.Scotty
import qualified Templates

import Data.Monoid (mconcat)

main :: IO ()
main = scotty 3000 $ do
  get "/" $ do
    html Templates.homepage
  get "/:word" $ do
    beam <- param "word"
    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
