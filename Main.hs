import Web.Scotty
import Network.Wai

import qualified Templates
import qualified Auth
import qualified Models

import qualified Data.Text.Lazy as L

import Data.Monoid (mconcat)

showToHtml :: Show a => a -> ActionM ()
showToHtml = html . L.pack . show

main :: IO ()
main = do
  scotty 3000 $ do
    get "/" $ do
      req <- request
      html $ Templates.homepage (Auth.loadSession req)
      showToHtml $ Auth.reqToMTokenTuple req
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
