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
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON LinksElt where
  parseJSON (Object v) = LinksElt <$> v .: "href" <*> v .: "rel"
  parseJSON _          = mzero


instance ToJSON LinksElt where
  toJSON (LinksElt {..}) =
    object ["href" .= linksEltHref, "rel" .= linksEltRel]
  toEncoding (LinksElt {..}) =
    pairs ("href" .= linksEltHref <> "rel" .= linksEltRel)


data Result = Result {
    resultStatus    :: Text,
    resultCreatedAt :: Text,
    resultChainId   :: Text,
    resultExtIds    :: [Text],
    resultLinks     :: [LinksElt],
    resultSynced    :: Bool
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Result where
  parseJSON (Object v) =
    Result
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


instance ToJSON Result where
  toJSON (Result {..}) = object
    [ "status" .= resultStatus
    , "createdAt" .= resultCreatedAt
    , "chainId" .= resultChainId
    , "extIds" .= resultExtIds
    , "links" .= resultLinks
    , "synced" .= resultSynced
    ]
  toEncoding (Result {..}) = pairs
    (  "status"
    .= resultStatus
    <> "createdAt"
    .= resultCreatedAt
    <> "chainId"
    .= resultChainId
    <> "extIds"
    .= resultExtIds
    <> "links"
    .= resultLinks
    <> "synced"
    .= resultSynced
    )


data Chain = Chain {
    topLevelResult :: Result
  } deriving (Show,Eq,GHC.Generics.Generic)


instance FromJSON Chain where
  parseJSON (Object v) = Chain <$> v .: "result"
  parseJSON _          = mzero


instance ToJSON Chain where
  toJSON (Chain {..}) = object ["result" .= topLevelResult]
  toEncoding (Chain {..}) = pairs ("result" .= topLevelResult)
