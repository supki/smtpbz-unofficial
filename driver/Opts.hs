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
  = OptsSmtpSend SmtpSend
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
    (command "smtp-send" (info (fmap OptsSmtpSend smtpSend) (progDesc "Send an email")))

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
