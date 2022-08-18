{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Cfg
  ( Cfg(..)
  , get
  , usageHeader
  , version
  ) where

import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Env
import qualified Network.HTTP.Conduit as Http

import qualified Meta_smtpbz_bin as Meta
import           Smtpbz (Has(..))


data Cfg = Cfg
  { cfgApiKey  :: ByteString
  , cfgBaseUrl :: Text
  , cfgMan     :: Http.Manager
  }

instance Has Cfg where
  apiKey l cfg =
    fmap (\key -> cfg {cfgApiKey = key}) (l (cfgApiKey cfg))
  {-# INLINE apiKey #-}
  baseUrl l cfg =
    fmap (\url -> cfg {cfgBaseUrl = url}) (l (cfgBaseUrl cfg))
  {-# INLINE baseUrl #-}
  httpMan l cfg =
    fmap (\man -> cfg {cfgMan = man}) (l (cfgMan cfg))
  {-# INLINE httpMan #-}

get :: IO Cfg
get = do
  cfgMan <- Http.newManager Http.tlsManagerSettings
  Env.parse (header usageHeader) . prefixed "SMTPBZ_" $ do
    cfgApiKey <-
      var str "API_KEY" (help "API key")
    cfgBaseUrl <-
      var str "BASE_URL" (help "Base URL" <> def "https://api.smtp.bz/v1" <> helpDef show)
    pure Cfg {..}

usageHeader :: String
usageHeader =
  unwords [Meta.name, version]

version :: String
version =
  Meta.version <> "-" <> Meta.hash
