{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.Rest.Client.Types.User where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), decode, object,
                                                  pairs, (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid
import           Data.Text                       (Text)
import qualified GHC.Generics
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

--------------------------------------------------------------------------------
-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

data User = User {
    userAccessToken :: Text,
    userName        :: Text,
    userUsage       :: Double,
    userUsageLimit  :: Double
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON User where
  parseJSON (Object v) =
    User
      <$> v
      .:  "accessToken"
      <*> v
      .:  "name"
      <*> v
      .:  "usage"
      <*> v
      .:  "usageLimit"
  parseJSON _ = mzero


instance ToJSON User where
  toJSON (User {..}) = object
    [ "accessToken" .= userAccessToken
    , "name" .= userName
    , "usage" .= userUsage
    , "usageLimit" .= userUsageLimit
    ]
  toEncoding (User {..}) = pairs
    (  "accessToken"
    .= userAccessToken
    <> "name"
    .= userName
    <> "usage"
    .= userUsage
    <> "usageLimit"
    .= userUsageLimit
    )
