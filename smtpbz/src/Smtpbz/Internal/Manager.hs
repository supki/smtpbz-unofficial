module Smtpbz.Internal.Manager
  ( Manager(..)
  , newManager
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Default (def)
import Network.HTTP.Conduit qualified as Http
import Network.Connection (TLSSettings(..))
import Network.TLS (Supported(..), EMSMode(AllowEMS), clientSupported)


-- A thin wrapper over 'Http.Manager' that API methods require.
--
-- Use 'newManager' to create a 'Manager' that is functionally equivalent to the
-- the default 'Http.Manager' but compatible with smtpbz servers. Alternatively,
-- you can wrap your own 'Http.Manager' if you're certain that it is compatible
-- with smtpbz servers.
newtype Manager = Manager { unManager :: Http.Manager }

-- | Create a 'Manager' that is functionally equivalent to the default 'Http.Manager'
-- but compatible with smtpbz servers.
--
-- Currently, this means relaxing the Extended Main Secret extension requirement.
newManager :: MonadIO m => m Manager
newManager =
  fmap Manager (liftIO (Http.newManager (Http.mkManagerSettings tlsSettings Nothing)))

-- | I'm unsure why http-client's API is so bad, but
-- we have to do a little bit of dancing here to cover
-- every possibility.
tlsSettings :: TLSSettings
tlsSettings =
  case defaultTlsSettings of
    settings@(TLSSettingsSimple {settingClientSupported = supported}) ->
      settings {settingClientSupported = supported {supportedExtendedMainSecret = AllowEMS}}
    TLSSettings params ->
      TLSSettings params {clientSupported = (clientSupported params) {supportedExtendedMainSecret = AllowEMS}}

defaultTlsSettings :: TLSSettings
defaultTlsSettings =
  def
