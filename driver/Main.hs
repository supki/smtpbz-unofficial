module Main (main) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)

import qualified Cfg
import qualified Opts
import           Opts (Opts(..))

import qualified Smtpbz


main :: IO ()
main = do
  cfg <- Cfg.get
  opts <- Opts.get
  res <- case opts of
    OptsUser ->
      Smtpbz.user cfg
    OptsUserStats ->
      Smtpbz.userStats cfg
    OptsUserDomains ->
      Smtpbz.userDomains cfg
    OptsUserDomain domain ->
      Smtpbz.userDomain cfg domain
    OptsUserIPs ->
      Smtpbz.userIPs cfg
    OptsUserIP ip ->
      Smtpbz.userIP cfg ip
    OptsLogMessages logMessages ->
      Smtpbz.logMessages cfg logMessages
    OptsLogMessage messageID ->
      Smtpbz.logMessage cfg messageID
    OptsUnsubscribe unsubscribe ->
      Smtpbz.unsubscribe cfg unsubscribe
    OptsUnsubscribeAdd address ->
      Smtpbz.unsubscribeAdd cfg address
    OptsUnsubscribeRemove address ->
      Smtpbz.unsubscribeRemove cfg address
    OptsUnsubscribeRemoveAll ->
      Smtpbz.unsubscribeRemoveAll cfg
    OptsSmtpSend cmdOpts ->
      Smtpbz.sendSmtp cfg cmdOpts
    OptsCheckEmail address ->
      Smtpbz.checkEmail cfg address
  Smtpbz.debugPrintResponse res
  unless (Smtpbz.successfulCall res) exitFailure
