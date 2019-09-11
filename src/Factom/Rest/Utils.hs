{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Factom.Rest.Utils where

import           Data.Aeson       (FromJSON (..), ToJSON (..), Value,
                                   genericParseJSON, genericToJSON)
import           Data.Aeson.Types (Options (..), defaultOptions)
import           Data.Function    ((&))
import           Data.List        (stripPrefix)
import qualified Data.Map         as Map
import           Data.Maybe       (fromMaybe)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           GHC.Generics     (Generic)

--------------------------------------------------------------------------------
