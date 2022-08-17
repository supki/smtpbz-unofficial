{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Smtpbz.Internal.Api
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

import           Smtpbz.Internal.Has (Has(..), view)


user :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
user smtpbz =
  simpleApiCall smtpbz "user"

userStats :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
userStats smtpbz =
  simpleApiCall smtpbz "user/stats"

userDomains :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
userDomains smtpbz =
  simpleApiCall smtpbz "user/domain"

userDomain :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
userDomain smtpbz domain =
  simpleApiCall smtpbz (printf "user/domain/%s" domain)

userIPs :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
userIPs smtpbz =
  simpleApiCall smtpbz "user/ip"

userIP :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
userIP smtpbz ip = do
  simpleApiCall smtpbz (printf "user/ip/%s" ip)

data LogMessages = LogMessages
  { limit  :: Maybe Int
  , offset :: Maybe Int
  , from   :: Maybe ByteString
  , to     :: Maybe ByteString
  , isOpen :: Maybe Bool
  , tag    :: Maybe ByteString
  } deriving (Show, Eq)

logMessages :: Has smtpbz => smtpbz -> LogMessages -> IO (Http.Response Lazy.ByteString)
logMessages smtpbz LogMessages {..} = do
  req <- prepareApiCall smtpbz "log/message"
  callApi smtpbz (Http.setQueryString params req)
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

logMessage :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
logMessage smtpbz messageID = do
  simpleApiCall smtpbz (printf "log/message/%s" messageID)

data Unsubscribe = Unsubscribe
  { limit   :: Maybe Int
  , offset  :: Maybe Int
  , address :: Maybe ByteString
  , reason  :: Maybe ByteString
  } deriving (Show, Eq)

unsubscribe :: Has smtpbz => smtpbz -> Unsubscribe -> IO (Http.Response Lazy.ByteString)
unsubscribe smtpbz Unsubscribe {..} = do
  req <- prepareApiCall smtpbz "unsubscribe"
  callApi smtpbz (Http.setQueryString params req)
 where
  params =
    [ ("limit", fmap (fromString . show) limit)
    , ("offset", fmap (fromString . show) offset)
    , ("address", address)
    , ("reason", reason)
    ]

unsubscribeAdd :: Has smtpbz => smtpbz -> ByteString -> IO (Http.Response Lazy.ByteString)
unsubscribeAdd smtpbz address = do
  req <- prepareApiCall smtpbz "unsubscribe/add"
  callApi smtpbz (Http.urlEncodedBody params req)
 where
  params =
    [ ("address", address)
    ]

unsubscribeRemove :: Has smtpbz => smtpbz -> ByteString -> IO (Http.Response Lazy.ByteString)
unsubscribeRemove smtpbz address = do
  req <- prepareApiCall smtpbz "unsubscribe/remove"
  callApi smtpbz (Http.urlEncodedBody params req)
 where
  params =
    [ ("address", address)
    ]

unsubscribeRemoveAll :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
unsubscribeRemoveAll smtpbz = do
  req <- prepareApiCall smtpbz "unsubscribe/removeall"
  callApi smtpbz (Http.urlEncodedBody [] req)

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

sendSmtp :: Has smtpbz => smtpbz -> SmtpSend -> IO (Http.Response Lazy.ByteString)
sendSmtp smtpbz SmtpSend {..} = do
  req <- prepareApiCall smtpbz "smtp/send"
  callApi smtpbz (Http.urlEncodedBody (collapse params) req)
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

checkEmail :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
checkEmail smtpbz email =
  simpleApiCall smtpbz (printf "check/email/%s" email)

simpleApiCall :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
simpleApiCall smtpbz path = do
  req <- prepareApiCall smtpbz path
  callApi smtpbz req

prepareApiCall :: Has smtpbz => smtpbz -> String -> IO Http.Request
prepareApiCall smtpbz path = do
  req <- Http.parseRequest (printf "%s/%s" (view baseUrl smtpbz) path)
  pure req
    { Http.requestHeaders = ("Authorization", view apiKey smtpbz) : Http.requestHeaders req
    }

callApi :: Has smtpbz => smtpbz -> Http.Request -> IO (Http.Response Lazy.ByteString)
callApi smtpbz req =
  Http.httpLbs req (view httpMan smtpbz)

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
