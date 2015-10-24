module Settings where

import Data.ByteString.Char8 (ByteString)

protocol = "http"
domain = "staging.threegoodthings.xyz" :: String
fullDomain = protocol ++ "://" ++ domain

mailgunKey = ""

port = 3000 :: Int

dbConnectionString = "dbname='threegoodhaskells'" :: ByteString

attachmentsBucket = ""
awsAccessKey = ""
awsSecretKey = ""
