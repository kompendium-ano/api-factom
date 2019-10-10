{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeOperators       #-}

module Factom.Rest.Client.Types.Chain where

import           Control.Applicative
import           Control.Monad                   (forM_, join, mzero)
import           Data.Aeson                      (FromJSON (..), ToJSON (..),
                                                  Value (..), decode, object,
                                                  pairs, (.:), (.:?), (.=))
import           Data.Aeson.AutoType.Alternative
import qualified Data.ByteString.Lazy.Char8      as BSL
import           Data.Monoid
import           Data.Text                       (Text)
import           Elm                             (ElmType)
import qualified GHC.Generics
import           System.Environment              (getArgs)
import           System.Exit                     (exitFailure, exitSuccess)
import           System.IO                       (hPutStrLn, stderr)

--------------------------------------------------------------------------------

-- | Workaround for https://github.com/bos/aeson/issues/287.
o .:?? val = fmap join (o .:? val)


data LinksElt = LinksElt {
    linksEltHref :: Text,
    linksEltRel  :: Text
  } deriving (Show,Eq,GHC.Generics.Generic, ElmType)


instance FromJSON LinksElt where
  parseJSON (Object v) = LinksElt <$> v .: "href" <*> v .: "rel"
  parseJSON _          = mzero


instance ToJSON LinksElt where
  toJSON (LinksElt {..}) =
    object ["href" .= linksEltHref, "rel" .= linksEltRel]
  toEncoding (LinksElt {..}) =
    pairs ("href" .= linksEltHref <> "rel" .= linksEltRel)


data ChainEntity = ChainEntity {
    chainEntStatus    :: Text,
    chainEntCreatedAt :: Text,
    chainEntChainId   :: Text,
    chainEntExtIds    :: [Text],
    chainEntLinks     :: [LinksElt],
    chainEntSynced    :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic, ElmType)


instance FromJSON ChainEntity where
  parseJSON (Object v) =
    ChainEntity
      <$> v
      .:  "status"
      <*> v
      .:  "createdAt"
      <*> v
      .:  "chainId"
      <*> v
      .:  "extIds"
      <*> v
      .:  "links"
      <*> v
      .:  "synced"
  parseJSON _ = mzero


instance ToJSON ChainEntity where
  toJSON (ChainEntity {..}) = object
    [ "status" .= chainEntStatus
    , "createdAt" .= chainEntCreatedAt
    , "chainId" .= chainEntChainId
    , "extIds" .= chainEntExtIds
    , "links" .= chainEntLinks
    , "synced" .= chainEntSynced
    ]
  toEncoding (ChainEntity {..}) = pairs
    (  "status"
    .= chainEntStatus
    <> "createdAt"
    .= chainEntCreatedAt
    <> "chainId"
    .= chainEntChainId
    <> "extIds"
    .= chainEntExtIds
    <> "links"
    .= chainEntLinks
    <> "synced"
    .= chainEntSynced
    )


data Chain = Chain {
    chainChainEntity :: ChainEntity
  } deriving (Show,Eq,GHC.Generics.Generic, ElmType)


instance FromJSON Chain where
  parseJSON (Object v) = Chain <$> v .: "chainEnt"
  parseJSON _          = mzero


instance ToJSON Chain where
  toJSON (Chain {..}) = object ["chainEnt" .= chainChainEntity]
  toEncoding (Chain {..}) = pairs ("chainEnt" .= chainChainEntity)
