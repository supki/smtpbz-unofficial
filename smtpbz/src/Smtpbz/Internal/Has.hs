{-# LANGUAGE RankNTypes #-}
module Smtpbz.Internal.Has
  ( Has(..)
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

import Smtpbz.Internal.Manager (Manager)


-- | smtp.bz configuration that is used by the library
-- and the user may want to override.
class Has t where
  -- | API key; get one at https://smtp.bz/panel/user
  apiKey :: t -> ByteString
  -- | API base URL; Most likely, https://api.smtp.bz/v1
  baseUrl :: t -> Text
  -- | The Manager under which all requests will be made.
  --
  -- The default Manager is incompatible with outdated smtpbz servers because
  -- it requires the Extended Main Secret extension, which those servers do not support.
  -- Therefore, we define a newtype to prevent unintentional passing of an incompatible
  -- Manager to API methods, which would result in a runtime error.
  httpMan :: t -> Manager
