{-# LANGUAGE RecordWildCards #-}
module Api
  ( SmtpSend(..)
  , user
  , userStats
  , userDomains
  , userDomain
  , userIPs
  , userIP
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

user :: Cfg -> IO (Http.Response Lazy.ByteString)
user cfg =
  simpleApiCall cfg "user"

userStats :: Cfg -> IO (Http.Response Lazy.ByteString)
userStats cfg =
  simpleApiCall cfg "user/stats"

userDomains :: Cfg -> IO (Http.Response Lazy.ByteString)
userDomains cfg =
  simpleApiCall cfg "user/domain"

userDomain :: Cfg -> String -> IO (Http.Response Lazy.ByteString)
userDomain cfg domain =
  simpleApiCall cfg (printf "user/domain/%s" domain)

userIPs :: Cfg -> IO (Http.Response Lazy.ByteString)
userIPs cfg =
  simpleApiCall cfg "user/ip"

userIP :: Cfg -> String -> IO (Http.Response Lazy.ByteString)
userIP cfg ip = do
  simpleApiCall cfg (printf "user/ip/%s" ip)

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

simpleApiCall :: Cfg -> String -> IO (Http.Response Lazy.ByteString)
simpleApiCall cfg path = do
  req <- prepareApiCall cfg path
  callApi cfg req

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
