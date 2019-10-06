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

endpoint = "http://localhost:8081/v1"

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

-- methods beyond current DeFacto specification
type FactomAPIExtended =

   -- entries from to
      "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> Capture "from" Int
    :> Capture "to"   Int
    :> Get '[JSON] (Either String  Entry)

  -- user chains from to
  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> Capture "from" Int
    :> Capture "to"   Int
    :> Get '[JSON] (Either String  Entry)


--------------------------------------------------------------------------------

getUser :: Maybe T.Text -> ClientM User
getChains :: Maybe T.Text -> ClientM (Either String  [Chain])
createChain :: Maybe T.Text -> ClientM (Either String  Chain)
getChain :: Maybe T.Text -> ClientM (Either String  Chain)
(     getUser
 :<|> getUserChains
 :<|> createChain
 :<|> getChainById
 :<|> getChainEntries
 :<|> getChainEntryFirst
 :<|> getChainEntryLast
 :<|> searchChainsByExternalIds
 :<|> searchEntriesChainByExternalIds
  ) = client factomAPI

--------------------------------------------------------------------------------

-- getUser
getUser' = undefined
getUserChains' = undefined
createChain' = undefined
getChain' = undefined
