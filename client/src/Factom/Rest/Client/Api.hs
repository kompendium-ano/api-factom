{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Factom.Rest.Client.Api where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.HTTP.Client        (Manager, newManager)
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Conduit       (simpleHttp)
import qualified Network.HTTP.Simple        as S
import           Servant.API
import           Servant.Client

import           Factom.Rest.Types
import           Factom.Rest.Utils

--------------------------------------------------------------------------------

endpoint = "http://localhost:8081/v1"

type FactomAPI =
       "user"
    :> Header "Authorization Bearer" T.Text
    :> Get '[JSON] User

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Get '[JSON] (ApiResponse [Chain])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Post '[JSON] (ApiResponse Chain)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> Post '[JSON] (ApiResponse Chain)

  :<|> ""

factomAPI :: Proxy FactomAPI
factomAPI = Proxy

getUser :: Maybe T.Text -> ClientM User
getChains :: Maybe T.Text -> ClientM (ApiResponse [Chain])
createChain :: Maybe T.Text -> ClientM (ApiResponse Chain)
getChain :: Maybe T.Text -> ClientM (ApiResponse Chain)
(     getUser
 :<|> getChains
 :<|> createChain
 :<|> getChain ) = client factomAPI

--------------------------------------------------------------------------------

getUser' = undefined
getChains' = undefined
createChain' = undefined
getChain' = undefined
