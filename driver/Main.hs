module Main (main) where

import           Data.Bool (bool)
import           System.Exit (exitFailure, exitSuccess)

import qualified Api
import qualified Cfg
import qualified Opts
import           Opts (Opts(..))


main :: IO ()
main = do
  cfg <- Cfg.get
  opts <- Opts.get
  res <- case opts of
    OptsSmtpSend cmdOpts ->
      Api.sendSmtp cfg cmdOpts
  Api.debugPrintResponse res
  bool exitFailure exitSuccess (Api.successfulCall res)
