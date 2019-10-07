{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.Rest.Client.Types.Entry where

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

--------------------------------------------------------------------------------x

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)

data Entry = Entry {
    entryStatus    :: Text,
    entryCreatedAt :: Text,
    entryChainId   :: Text,
    entryEntryHash :: Text,
    entryContent   :: Text,
    entryExtIds    :: [Text]
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Entry where
  parseJSON (Object v) =
    Entry
      <$> v
      .:  "status"
      <*> v
      .:  "createdAt"
      <*> v
      .:  "chainId"
      <*> v
      .:  "entryHash"
      <*> v
      .:  "content"
      <*> v
      .:  "extIds"
  parseJSON _ = mzero


instance ToJSON Entry where
  toJSON (Entry {..}) = object
    [ "status" .= entryStatus
    , "createdAt" .= entryCreatedAt
    , "chainId" .= entryChainId
    , "entryHash" .= entryEntryHash
    , "content" .= entryContent
    , "extIds" .= entryExtIds
    ]
  toEncoding (Entry {..}) = pairs
    (  "status"
    .= entryStatus
    <> "createdAt"
    .= entryCreatedAt
    <> "chainId"
    .= entryChainId
    <> "entryHash"
    .= entryEntryHash
    <> "content"
    .= entryContent
    <> "extIds"
    .= entryExtIds
    )
