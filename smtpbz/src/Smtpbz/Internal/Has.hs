{-# LANGUAGE RankNTypes #-}
module Smtpbz.Internal.Has
  ( Has(..)
  , Lens
  , Lens'
  , view
  ) where

import           Data.ByteString (ByteString)
import           Data.Functor.Const (Const(..))
import           Data.Text (Text)
import qualified Network.HTTP.Conduit as Http


-- | smtp.bz configuration that is used by the library
-- and the user may want to override.
class Has t where
  -- | API key; get one at https://smtp.bz/panel/user
  apiKey :: Lens' t ByteString
  -- | API base URL; Most likely, https://api.smtp.bz/v1
  baseUrl :: Lens' t Text
  -- | The 'Http.Manager' under which all requests will be made.
  httpMan :: Lens' t Http.Manager

-- | Type changing stabby lenses.
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | Stabby lenses that can't change the type.
type Lens' s a = Lens s s a a

view :: Lens s t a b -> s -> a
view l x = getConst (l Const x)
{-# INLINE view #-}
