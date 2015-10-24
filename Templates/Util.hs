module Templates.Util (postToHtml, attachmentsToHtml, textToHtml) where

import Lucid
import Data.String
import Database.PostgreSQL.Simple.Time (Unbounded(Finite))
import Control.Monad (void)
import qualified Data.Text as Text

import Models
import Time (formatPostDate)

postToHtml :: Post -> [Attachment] -> Html ()
postToHtml (Post _ (Just text) (Finite timestamp) _ _) as = do
  h4_ $ toHtml $ formatPostDate timestamp
  textToHtml text
  attachmentsToHtml as
postToHtml _ _ = void ""

attachmentsToHtml :: [Attachment] -> Html ()
attachmentsToHtml as = mapM_ (\(Attachment _ url _ _) ->
          p_ $ img_ [src_ $ Text.pack url]
          ) as

textToHtml :: String -> Html ()
textToHtml text = mapM_ (p_ . toHtml) (Data.String.lines text)
