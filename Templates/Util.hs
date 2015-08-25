module Templates.Util (postToHtml, textToHtml) where

import Lucid
import Data.String
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Control.Monad (void)

import Models
import Time (formatPostDate)

postToHtml :: Post -> Html ()
postToHtml (Post _ (Just text) (Finite timestamp) _ _) = do
  h4_ $ toHtml $ formatPostDate timestamp
  textToHtml text
postToHtml _ = void ""

textToHtml :: String -> Html ()
textToHtml text = mapM_ (p_ . toHtml) (Data.String.lines text)
