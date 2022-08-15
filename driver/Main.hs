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
    OptsSmtpSend cmdOpts ->
      Api.sendSmtp cfg cmdOpts
  Api.debugPrintResponse res
  unless (Api.successfulCall res) exitFailure
