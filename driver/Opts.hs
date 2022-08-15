{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Opts
  ( Opts(..)
  , SmtpSend(..)
  , get
  ) where

import Options.Applicative

import Cfg (usageHeader)
import Api (SmtpSend(..))


data Opts
  = OptsUser
  | OptsUserStats
  | OptsUserDomains
  | OptsUserDomain String
  | OptsUserIPs
  | OptsUserIP String
  | OptsSmtpSend SmtpSend
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
    ( command "user" (info (pure OptsUser) (progDesc "User data"))
   <> command "user-stats" (info (pure OptsUserStats) (progDesc "User mail stats"))
   <> command "user-domains" (info (pure OptsUserDomains) (progDesc "All domains data"))
   <> command "user-domain" (info (fmap OptsUserDomain userDomain) (progDesc "Domain data"))
   <> command "user-ips" (info (pure OptsUserIPs) (progDesc "All IPs data"))
   <> command "user-ip" (info (fmap OptsUserIP userIP) (progDesc "IP data"))

   <> command "smtp-send" (info (fmap OptsSmtpSend smtpSend) (progDesc "Send an email"))
    )

userDomain :: Parser String
userDomain =
  argument str (metavar "DOMAIN")

userIP :: Parser String
userIP =
  argument str (metavar "IP")

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
