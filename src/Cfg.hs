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

import qualified Meta_smtpbz as Meta


data Cfg = Cfg
  { cfgApiKey  :: ByteString
  , cfgBaseUrl :: Text
  , cfgMan     :: Http.Manager
  }

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
