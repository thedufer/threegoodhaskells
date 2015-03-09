module Util (maybeRead) where

import Control.Monad (liftM)
import Data.Maybe (listToMaybe)

maybeRead :: Read a => String -> Maybe a
maybeRead = (liftM fst) . listToMaybe . reads
