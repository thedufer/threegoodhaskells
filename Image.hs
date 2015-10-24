module Image (isImage, fileToUrl) where

import Models
import RandomStrings
import Settings

import Data.Either (isRight)
import Data.Maybe (Maybe())
import Web.Scotty
import Network.Wai.Parse (fileContent, fileContentType, fileName)
import Network.AWS.AWSConnection hiding (awsAccessKey, awsSecretKey)
import Network.AWS.S3Object
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Network.AWS.AWSResult

imageMimeTypes :: [BS.ByteString]
imageMimeTypes = ["image/bmp", "image/gif", "image/jpeg", "image/png", "image/tiff"]

isImage :: File -> Bool
isImage (_, info) = (fileContentType info) `elem` imageMimeTypes

awsConn :: AWSConnection
awsConn = AWSConnection defaultAmazonS3Host defaultAmazonS3Port awsAccessKey awsSecretKey

fileToUrl :: File -> IO (Maybe Url)
fileToUrl (_, info) = do
  token <- generateHex32
  name <- return $ token ++ "/" ++ (BSC.unpack $ fileName info)
  url <- return $ "http://" ++ attachmentsBucket ++ ".s3.amazonaws.com/" ++ name
  obj <- return $ S3Object attachmentsBucket name (BSC.unpack $ fileContentType info) [] (fileContent info)
  res <- (sendObject awsConn $ obj)
  case res of
    Right _ -> return (Just url)
    Left err -> case err of
      NetworkError _ -> do
        print "Network Error"
        return Nothing
      AWSError str1 str2 -> do
        print "AWSError"
        print str1
        print str2
        return Nothing
