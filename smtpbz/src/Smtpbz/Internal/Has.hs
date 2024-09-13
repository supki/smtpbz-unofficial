{-# LANGUAGE RankNTypes #-}
module Smtpbz.Internal.Has
  ( Has(..)
  ) where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import qualified Network.HTTP.Conduit as Http


-- | smtp.bz configuration that is used by the library
-- and the user may want to override.
class Has t where
  -- | API key; get one at https://smtp.bz/panel/user
  apiKey :: t -> ByteString
  -- | API base URL; Most likely, https://api.smtp.bz/v1
  baseUrl :: t -> Text
  -- | The 'Http.Manager' under which all requests will be made.
  httpMan :: t -> Http.Manager
