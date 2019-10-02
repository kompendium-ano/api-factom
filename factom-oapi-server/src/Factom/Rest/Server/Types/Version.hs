{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.Rest.Server.Version where

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


data Result = Result {
    resultVersion :: Text
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Result where
  parseJSON (Object v) = Result <$> v .:   "version"
  parseJSON _          = mzero


instance ToJSON Result where
  toJSON     (Result {..}) = object ["version" .= resultVersion]
  toEncoding (Result {..}) = pairs  ("version" .= resultVersion)


data VersionResult = VersionResult {
    topLevelResult :: Result
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON VersionResult where
  parseJSON (Object v) = VersionResult <$> v .:   "result"
  parseJSON _          = mzero


instance ToJSON VersionResult where
  toJSON     (VersionResult {..}) = object ["result" .= topLevelResult]
  toEncoding (VersionResult {..}) = pairs  ("result" .= topLevelResult)
