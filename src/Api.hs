{-# LANGUAGE RecordWildCards #-}
module Api
  ( SmtpSend(..)
  , sendSmtp
  , successfulCall
  , debugPrintResponse
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import           Data.Maybe (mapMaybe)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Types as Http
import           Text.Printf (printf)

import           Cfg (Cfg(..))


data SmtpSend = SmtpSend
  { from    :: ByteString
  , name    :: Maybe ByteString
  , subject :: ByteString
  , to      :: ByteString
  , replyTo :: Maybe ByteString
  , html    :: ByteString
  , text    :: Maybe ByteString
  -- , headers :: ByteString ???
  } deriving (Show, Eq)

sendSmtp :: Cfg -> SmtpSend -> IO (Http.Response Lazy.ByteString)
sendSmtp cfg SmtpSend {..} = do
  req <- prepareApiCall cfg "smtp/send"
  callApi cfg (Http.urlEncodedBody params req)
 where
  params =
    -- :: [(a, Maybe b)] -> [(a, b)]
    mapMaybe sequence
      [ ("from", pure from)
      , ("name", name)
      , ("subject", pure subject)
      , ("to", pure to)
      , ("reply", replyTo)
      , ("html", pure html)
      , ("text", text)
      ]

prepareApiCall :: Cfg -> String -> IO Http.Request
prepareApiCall Cfg {..} path = do
  req <- Http.parseRequest (printf "%s/%s" cfgBaseUrl path)
  pure req
    { Http.requestHeaders = ("Authorization", cfgApiKey) : Http.requestHeaders req
    }

callApi :: Cfg -> Http.Request -> IO (Http.Response Lazy.ByteString)
callApi Cfg {..} req =
  Http.httpLbs req cfgMan

successfulCall :: Http.Response Lazy.ByteString -> Bool
successfulCall res =
  case Http.responseStatus res of
    st ->
      Http.status200 <= st && st < Http.status300

debugPrintResponse :: Http.Response Lazy.ByteString -> IO ()
debugPrintResponse =
  ByteString.Lazy.putStrLn . Http.responseBody
