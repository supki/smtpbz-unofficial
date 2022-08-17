{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Opts
  ( Opts(..)
  , SmtpSend(..)
  , get
  ) where

import Data.ByteString (ByteString)
import Options.Applicative

import Smtpbz (LogMessages(..), Unsubscribe(..), SmtpSend(..))

import Cfg (usageHeader)


data Opts
  = OptsUser
  | OptsUserStats
  | OptsUserDomains
  | OptsUserDomain String
  | OptsUserIPs
  | OptsUserIP String
  | OptsLogMessages LogMessages
  | OptsLogMessage String
  | OptsUnsubscribe Unsubscribe
  | OptsUnsubscribeAdd ByteString
  | OptsUnsubscribeRemove ByteString
  | OptsUnsubscribeRemoveAll
  | OptsSmtpSend SmtpSend
  | OptsCheckEmail String
    deriving (Show, Eq)

get :: IO Opts
get =
  execParser
    (info
      (parser <**> helper)
      (fullDesc <> header usageHeader <> progDesc "Unofficial smtp.bz API client"))

parser :: Parser Opts
parser =
  subparser
    ( command "user"
        (info (pure OptsUser) (progDesc "User data"))
   <> command "user-stats"
        (info (pure OptsUserStats) (progDesc "User mail stats"))
   <> command "user-domains"
        (info (pure OptsUserDomains) (progDesc "All domains data"))
   <> command "user-domain"
        (info (fmap OptsUserDomain userDomain) (progDesc "Domain data"))
   <> command "user-ips"
        (info (pure OptsUserIPs) (progDesc "All IPs data"))
   <> command "user-ip"
        (info (fmap OptsUserIP userIP) (progDesc "IP data"))

   <> command "log-messages"
        (info (fmap OptsLogMessages logMessages) (progDesc "Get sent e-mails"))
   <> command "log-message"
        (info (fmap OptsLogMessage logMessage) (progDesc "Get sent e-mail by ID"))

   <> command "unsubscribe"
        (info (fmap OptsUnsubscribe unsubscribe) (progDesc "Get unsubscribed e-mail addresses"))
   <> command "unsubscribe-add"
        (info (fmap OptsUnsubscribeAdd unsubscribeAdd) (progDesc "Unsubscribe e-mail address"))
   <> command "unsubscribe-remove"
        (info
          (fmap OptsUnsubscribeRemove unsubscribeRemove)
          (progDesc "Remove e-mail address from unsubscribed"))
   <> command "unsubscribe-remove-all"
        (info (pure OptsUnsubscribeRemoveAll) (progDesc "Clear unsubscribed e-email addresses"))

   <> command "smtp-send"
        (info (fmap OptsSmtpSend smtpSend) (progDesc "Send an e-mail"))

   <> command "check-email"
        (info (fmap OptsCheckEmail checkEmail) (progDesc "Validate e-mail address"))
    )

userDomain :: Parser String
userDomain =
  argument str (metavar "DOMAIN")

userIP :: Parser String
userIP =
  argument str (metavar "IP")

logMessages :: Parser LogMessages
logMessages = do
  limit <-
    optional (option auto (long "limit"))
  offset <-
    optional (option auto (long "offset"))
  from <-
    optional (strOption (long "from" <> metavar "ADDRESS" <> help "Sender's e-mail address"))
  to <-
    optional (strOption (long "to" <> metavar "ADDRESS" <> help "Receiver's e-mail address"))
  isOpen <-
    optional (option auto (long "is-open" <> help "Only opened e-mails"))
  tag <-
    optional (strOption (long "tag" <> metavar "X-TAG"))
  pure LogMessages {..}

logMessage :: Parser String
logMessage =
  argument str (metavar "ID")

unsubscribe :: Parser Unsubscribe
unsubscribe = do
  limit <-
    optional (option auto (long "limit"))
  offset <-
    optional (option auto (long "offset"))
  address <-
    optional (strOption (long "address" <> metavar "ADDRESS"))
  reason <-
    optional (strOption (long "reason" <> metavar "bounce|user|unsubscribe"))
  pure Unsubscribe {..}

unsubscribeAdd :: Parser ByteString
unsubscribeAdd =
  argument str (metavar "ADDRESS")

unsubscribeRemove :: Parser ByteString
unsubscribeRemove =
  argument str (metavar "ADDRESS")

smtpSend :: Parser SmtpSend
smtpSend = do
  from <-
    strOption (long "from" <> metavar "ADDRESS" <> help "Sender's e-mail address")
  name <-
    optional (strOption (long "name" <> help "Sender's name"))
  subject <-
    strOption (long "subject" <> help "Mail's subject")
  to <-
    strOption (long "to" <> metavar "ADDRESS" <> help "Receiver's e-mail address")
  replyTo <-
    optional (strOption (long "reply-to" <> metavar "ADDRESS" <> help "Address to reply to"))
  html <-
    strOption (long "html" <> help "Mail's text in HTML format")
  text <-
    optional (strOption (long "text" <> help "Mail's text in text format"))
  pure SmtpSend {..}

checkEmail :: Parser String
checkEmail =
  argument str (metavar "ADDRESS")
