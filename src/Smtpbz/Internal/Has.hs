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


class Has t where
  apiKey :: Lens' t ByteString
  baseUrl :: Lens' t Text
  httpMan :: Lens' t Http.Manager

type Lens' s a = Lens s s a a

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

view :: Lens s t a b -> s -> a
view l x = getConst (l Const x)
{-# INLINE view #-}
