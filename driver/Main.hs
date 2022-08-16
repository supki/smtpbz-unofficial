module Main (main) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)

import qualified Api
import qualified Cfg
import qualified Opts
import           Opts (Opts(..))


main :: IO ()
main = do
  cfg <- Cfg.get
  opts <- Opts.get
  res <- case opts of
    OptsUser ->
      Api.user cfg
    OptsUserStats ->
      Api.userStats cfg
    OptsUserDomains ->
      Api.userDomains cfg
    OptsUserDomain domain ->
      Api.userDomain cfg domain
    OptsUserIPs ->
      Api.userIPs cfg
    OptsUserIP ip ->
      Api.userIP cfg ip
    OptsLogMessages logMessages ->
      Api.logMessages cfg logMessages
    OptsLogMessage messageID ->
      Api.logMessage cfg messageID
    OptsUnsubscribe unsubscribe ->
      Api.unsubscribe cfg unsubscribe
    OptsUnsubscribeAdd address ->
      Api.unsubscribeAdd cfg address
    OptsUnsubscribeRemove address ->
      Api.unsubscribeRemove cfg address
    OptsUnsubscribeRemoveAll ->
      Api.unsubscribeRemoveAll cfg
    OptsSmtpSend cmdOpts ->
      Api.sendSmtp cfg cmdOpts
    OptsCheckEmail address ->
      Api.checkEmail cfg address
  Api.debugPrintResponse res
  unless (Api.successfulCall res) exitFailure
