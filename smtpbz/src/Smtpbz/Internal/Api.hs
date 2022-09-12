{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
-- | Reference: https://docs.smtp.bz/#/api
--
-- The response bodies are really un(der)specified, so this library doesn't
-- try to do to much with them. You are on your own.
--
-- Functions' names map (trivially) to API routes. So, for some of them,
-- it's not obvious what they do from their name. This is by design.
module Smtpbz.Internal.Api
  ( user
  , userStats
  , userDomains
  , userDomain
  , userIPs
  , userIP
  , LogMessages(..)
  , logMessages
  , logMessage
  , Unsubscribe(..)
  , unsubscribe
  , unsubscribeAdd
  , unsubscribeRemove
  , unsubscribeRemoveAll
  , SmtpSend(..)
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


-- | User data.
user :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
user smtpbz =
  simpleApiCall smtpbz "user"

-- | User's mail distribution statistics.
userStats :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
userStats smtpbz =
  simpleApiCall smtpbz "user/stats"

-- | User's domains data.
userDomains :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
userDomains smtpbz =
  simpleApiCall smtpbz "user/domain"

-- | User's specific domain data.
userDomain :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
userDomain smtpbz domain =
  simpleApiCall smtpbz (printf "user/domain/%s" domain)

-- | User's IPs data.
userIPs :: Has smtpbz => smtpbz -> IO (Http.Response Lazy.ByteString)
userIPs smtpbz =
  simpleApiCall smtpbz "user/ip"

-- | User's specific IP data.
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

-- | Message log search.
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

-- | Look up a specific message.
logMessage :: Has smtpbz => smtpbz -> String -> IO (Http.Response Lazy.ByteString)
logMessage smtpbz messageID = do
  simpleApiCall smtpbz (printf "log/message/%s" messageID)

data Unsubscribe = Unsubscribe
  { limit   :: Maybe Int
  , offset  :: Maybe Int
  , address :: Maybe ByteString
  , reason  :: Maybe ByteString
  } deriving (Show, Eq)

-- | List of e-mail addresses mail is not delivired to.
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

-- | Ignore an address.
unsubscribeAdd :: Has smtpbz => smtpbz -> ByteString -> IO (Http.Response Lazy.ByteString)
unsubscribeAdd smtpbz address = do
  req <- prepareApiCall smtpbz "unsubscribe/add"
  callApi smtpbz (Http.urlEncodedBody params req)
 where
  params =
    [ ("address", address)
    ]

-- | Stop ignoring an address.
unsubscribeRemove :: Has smtpbz => smtpbz -> ByteString -> IO (Http.Response Lazy.ByteString)
unsubscribeRemove smtpbz address = do
  req <- prepareApiCall smtpbz "unsubscribe/remove"
  callApi smtpbz (Http.urlEncodedBody params req)
 where
  params =
    [ ("address", address)
    ]

-- | Stop ignoring all previously ignored addresses.
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

--- | Send an email.
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

--- | Check email address validity.
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

-- | Check if response status code is in [200, 300).
successfulCall :: Http.Response Lazy.ByteString -> Bool
successfulCall res =
  case Http.responseStatus res of
    st ->
      Http.status200 <= st && st < Http.status300

-- | Print response body to stdout.
debugPrintResponse :: Http.Response Lazy.ByteString -> IO ()
debugPrintResponse =
  ByteString.Lazy.putStrLn . Http.responseBody

collapse :: [(a, Maybe b)] -> [(a, b)]
collapse =
  mapMaybe sequence
