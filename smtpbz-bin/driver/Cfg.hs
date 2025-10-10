{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}
module Cfg
  ( Cfg(..)
  , get
  , usageHeader
  , version
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Env

import Meta_smtpbz_bin qualified as Meta
import Smtpbz qualified


data Cfg = Cfg
  { cfgApiKey  :: ByteString
  , cfgBaseUrl :: Text
  , cfgMan     :: Smtpbz.Manager
  }

instance Smtpbz.Has Cfg where
  apiKey =
    cfgApiKey
  baseUrl =
    cfgBaseUrl
  httpMan =
    cfgMan

get :: IO Cfg
get = do
  cfgMan <- Smtpbz.newManager
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
