module Post (makePost, addToPost) where

import Models
import qualified Time

makePost :: Member -> IO Post
makePost = do
  date <- Time.currentPostDate

addToPost :: IO Integer
addToPost = return 1
