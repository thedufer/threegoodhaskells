module Mailgun (sendMessage) where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Network.Mail.Mime (Mail, renderMail')
import Network.HTTP.Client.MultipartFormData
import Network.HTTP.Conduit (withManager, httpLbs, Response, Request(..), Manager, parseUrl, applyBasicAuth)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text
import Data.Text.Encoding (encodeUtf8)
import Network.Mime (defaultMimeType)

baseUrl :: String
baseUrl = "https://api.mailgun.net/v2"

makeParts :: Text -> Mail -> IO [Part]
makeParts to mail = do
  mailLBS <- renderMail' mail
  messagePart <- return (partLBS "message" mailLBS)
  return [(partBS "to" (encodeUtf8 to)), messagePart { partFilename = (Just "message"), partContentType = (Just defaultMimeType) }]

sendMessage :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
                String -> String -> String -> Mail -> m (Response LBS.ByteString)
sendMessage domain apiKey to mail = do
  withManager $ \manager -> do
    sendWith manager domain apiKey to mail

sendWith :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) =>
            Manager -> String -> String -> String -> Mail -> m (Response LBS.ByteString)
sendWith manager domain apiKey to mail = do
  initReq <- parseUrl $ baseUrl ++ "/" ++ domain ++ "/messages.mime"
  let authReq = applyBasicAuth "api" (BS.pack apiKey) initReq
      postReq = authReq { method = "POST" }
  parts <- liftIO $ makeParts (pack to) mail
  res <- flip httpLbs manager =<<
    (formDataBody parts postReq)
  return res
