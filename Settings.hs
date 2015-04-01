module Settings where

import Data.ByteString.Char8 (ByteString)

protocol = ""
domain = "" :: String
fullDomain = protocol ++ "://" ++ domain

mailgunKey = ""

port = 3000 :: Int

dbConnectionString = "" :: ByteString
