import Web.Scotty
import Network.Wai
import qualified Templates
import Models

import Control.Monad (liftM)
import Data.Monoid (mconcat)

loadSession :: Request -> Maybe User
loadSession req = Just $ User "me"

main :: IO ()
main = do
  scotty 3000 $ do
    get "/" $ do
      req <- request
      html $ Templates.homepage (loadSession req)
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
