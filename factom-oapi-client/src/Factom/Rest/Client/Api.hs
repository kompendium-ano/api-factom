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

import           Factom.Rest.Client.Utils
import           Factom.Rest.Types

--------------------------------------------------------------------------------
-- Configuration related

endpoint = "http://localhost:8081/v1"

--------------------------------------------------------------------------------
-- Minimal API
--   methods according to current DeFacto's specification
--   https://docs.openapi.de-facto.pro/

type FactomAPIMinimal =
       "user"
    :> Header "Authorization Bearer" T.Text
    :> Get '[JSON] User

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Get '[JSON] (Either String [Chain])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Post '[JSON] (Either String [Chain])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> Post '[JSON] (Either String Chain)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> Post '[JSON] (Either String [Entry])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> Get '[JSON] (Either String [Entry])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> "first"
    :> Get '[JSON] (Either String  Entry)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> "last"
    :> Get '[JSON] (Either String  Entry)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> "search"
    :> Post '[JSON] (Either String Chain)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> "searh"
    :> Post '[JSON] (Either String Chain)


factomAPI :: Proxy FactomAPIMinimal
factomAPI = Proxy

--------------------------------------------------------------------------------
-- Extended API
--    methods beyond current DeFacto specification
--    implemented in Haskell OpenApi Server part

type FactomAPIExtended =

   -- entries from to
      "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> Capture "from" Int
    :> Capture "to"   Int
    :> Get '[JSON] (Either String [Entry])

  -- user chains from to
  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "from" Int
    :> Capture "to"   Int
    :> Get '[JSON] (Either String [Chain])

factomExtendedAPI :: Proxy MonobankCorporateAPI
factomExtendedAPI = Proxy

getEntriesRange :: Maybe T.Text -> T.Text -> Int -> Int -> ClientM (Either String  [Chain])
getChainsRange  :: Maybe T.Text -> Int -> Int -> ClientM (Either String  [Chain])
(     getEntries
 :<|> getChainsRange) = client factomExtendedAPI

--------------------------------------------------------------------------------

getUser :: Maybe T.Text -> ClientM User
getChains :: Maybe T.Text -> ClientM (Either String  [Chain])
createChain :: Maybe T.Text -> ClientM (Either String  Chain)
getChainById :: Maybe T.Text -> ClientM (Either String  Chain)
getChainEntries :: Maybe T.Text -> ClientM (Either String  Chain)
getChainEntryFirst :: Maybe T.Text -> ClientM (Either String  Chain)
getChainEntryLast :: Maybe T.Text -> ClientM (Either String  Chain)
searchChainsByExternalIds :: Maybe T.Text -> ClientM (Either String  Chain)
  searchEntriesChainByExternalIds
(     getUser
 :<|> getUserChains
 :<|> createChain
 :<|> getChainById
 :<|> getChainEntries
 :<|> getChainEntryFirst
 :<|> getChainEntryLast
 :<|> searchChainsByExternalIds
 :<|> searchEntriesChainByExternalIds ) = client factomAPIMinimal

--------------------------------------------------------------------------------

getUser' :: IO (Either ServantError User)
getUser' = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM getUser env
  where
    host = (BaseUrl Https endpoint 443 "")

getUserChains' = undefined
createChain' = undefined
getChainById' = undefined
getChainEntries' = undefined
getChainEntryFirst' = undefined
getChainEntryLast' = undefined
searchChainsByExternalIds' = undefined
searchEntriesChainByExternalIds' = undefined

getEntries' = undefined
getChainsRange' = undefined
