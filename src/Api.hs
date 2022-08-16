{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Api
  ( LogMessages(..)
  , SmtpSend(..)
  , Unsubscribe(..)
  , user
  , userStats
  , userDomains
  , userDomain
  , userIPs
  , userIP
  , logMessages
  , logMessage
  , unsubscribe
  , unsubscribeAdd
  , unsubscribeRemove
  , unsubscribeRemoveAll
  , sendSmtp
  , checkEmail
  , successfulCall
  , debugPrintResponse
  ) where

import           Data.Bool (bool)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy
import           Data.Maybe (mapMaybe)
import           Data.String (fromString)
import qualified Network.HTTP.Conduit as Http
import qualified Network.HTTP.Types as Http
import           Text.Printf (printf)

import           Cfg (Cfg(..))


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

data LogMessages = LogMessages
  { limit  :: Maybe Int
  , offset :: Maybe Int
  , from   :: Maybe ByteString
  , to     :: Maybe ByteString
  , isOpen :: Maybe Bool
  , tag    :: Maybe ByteString
  } deriving (Show, Eq)

logMessages :: Cfg -> LogMessages -> IO (Http.Response Lazy.ByteString)
logMessages cfg LogMessages {..} = do
  req <- prepareApiCall cfg "log/message"
  callApi cfg (Http.setQueryString params req)
 where
  params =
    [ ("limit", fmap (fromString . show) limit)
    , ("offset", fmap (fromString . show) offset)
    , ("from", from)
    , ("to", to)
      -- documentation says it's a normal person's bool,
      -- but in reality it's a C-programmer's bool
    , ("is_open", fmap (bool "0" "1") isOpen)
    , ("tag", tag)
    ]

logMessage :: Cfg -> String -> IO (Http.Response Lazy.ByteString)
logMessage cfg messageID = do
  simpleApiCall cfg (printf "log/message/%s" messageID)

data Unsubscribe = Unsubscribe
  { limit   :: Maybe Int
  , offset  :: Maybe Int
  , address :: Maybe ByteString
  , reason  :: Maybe ByteString
  } deriving (Show, Eq)

unsubscribe :: Cfg -> Unsubscribe -> IO (Http.Response Lazy.ByteString)
unsubscribe cfg Unsubscribe {..} = do
  req <- prepareApiCall cfg "unsubscribe"
  callApi cfg (Http.setQueryString params req)
 where
  params =
    [ ("limit", fmap (fromString . show) limit)
    , ("offset", fmap (fromString . show) offset)
    , ("address", address)
    , ("reason", reason)
    ]

unsubscribeAdd :: Cfg -> ByteString -> IO (Http.Response Lazy.ByteString)
unsubscribeAdd cfg address = do
  req <- prepareApiCall cfg "unsubscribe/add"
  callApi cfg (Http.urlEncodedBody params req)
 where
  params =
    [ ("address", address)
    ]

unsubscribeRemove :: Cfg -> ByteString -> IO (Http.Response Lazy.ByteString)
unsubscribeRemove cfg address = do
  req <- prepareApiCall cfg "unsubscribe/remove"
  callApi cfg (Http.urlEncodedBody params req)
 where
  params =
    [ ("address", address)
    ]

unsubscribeRemoveAll :: Cfg -> IO (Http.Response Lazy.ByteString)
unsubscribeRemoveAll cfg = do
  req <- prepareApiCall cfg "unsubscribe/removeall"
  callApi cfg (Http.urlEncodedBody [] req)

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
  callApi cfg (Http.urlEncodedBody (collapse params) req)
 where
  params =
    [ ("from", pure from)
    , ("name", name)
    , ("subject", pure subject)
    , ("to", pure to)
    , ("reply", replyTo)
    , ("html", pure html)
    , ("text", text)
    ]

checkEmail :: Cfg -> String -> IO (Http.Response Lazy.ByteString)
checkEmail cfg email =
  simpleApiCall cfg (printf "check/email/%s" email)

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

collapse :: [(a, Maybe b)] -> [(a, b)]
collapse =
  mapMaybe sequence
