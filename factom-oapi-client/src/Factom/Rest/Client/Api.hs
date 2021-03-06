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
import qualified Data.ByteString.Char8          as B
import qualified Data.ByteString.Lazy.Char8     as BL
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                      as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           Elm
import           GHC.Generics
import           Network.HTTP.Client            (Manager, newManager)
import qualified Network.HTTP.Client            as C
import           Network.HTTP.Client.TLS        (tlsManagerSettings)
import           Network.HTTP.Conduit           (simpleHttp)
import qualified Network.HTTP.Simple            as S
import           Servant.API
import           Servant.Client

import           Factom.Rest.Client.Types
import           Factom.Rest.Client.Types.Chain
import           Factom.Rest.Client.Types.Entry
import           Factom.Rest.Client.Types.User
import           Factom.Rest.Client.Utils

--------------------------------------------------------------------------------
-- Configuration related

--endpoint = "http://localhost:8081/v1"
--endpointRemote = "https://api.factom.kelecorix.com"

instance ElmType (Either String [Chain])
instance ElmType (Either String [Entry])
instance ElmType (Either String Chain)
instance ElmType (Either String Entry)

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
    :> QueryParam "start" Int
    :> QueryParam "limit" Int
    :> QueryParam "status" T.Text
    :> QueryParam "sort" T.Text
    :> Get '[JSON] (Either String [Chain])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> QueryParam "callback_url" T.Text
    :> ReqBody '[JSON] [(T.Text, T.Text)]
    :> Post '[JSON] (Either String Chain)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> Post '[JSON] (Either String Chain)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> QueryParam "start" Int
    :> QueryParam "limit" Int
    :> QueryParam "status" T.Text
    :> QueryParam "sort" T.Text
    :> Post '[JSON] (Either String [Entry])

  -- :<|> "chains"
  --   :> Header "Authorization Bearer" T.Text
  --   :> Capture "chainid" T.Text
  --   :> "entries"
  --   :> Get '[JSON] (Either String [Entry])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> "first"
    :> Get '[JSON] (Either String Entry)

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> "last"
    :> Get '[JSON] (Either String Entry)

  :<|> "chains"
    :> "search"
    :> Header "Authorization Bearer" T.Text
    :> QueryParam "start" Int
    :> QueryParam "limit" Int
    :> QueryParam "status" T.Text
    :> QueryParam "sort" T.Text
    :> ReqBody '[JSON] [T.Text]
    :> Post '[JSON] (Either String [Chain])

  :<|> "chains"
    :> Header "Authorization Bearer" T.Text
    :> Capture "chainid" T.Text
    :> "entries"
    :> "search"
    :> QueryParam "start" Int
    :> QueryParam "limit" Int
    :> QueryParam "status" T.Text
    :> QueryParam "sort" T.Text
    :> ReqBody '[JSON] [T.Text]
    :> Post '[JSON] (Either String [Entry])

factomAPIMinimal :: Proxy FactomAPIMinimal
factomAPIMinimal = Proxy

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

factomExtendedAPI :: Proxy FactomAPIExtended
factomExtendedAPI = Proxy

getEntriesRange :: Maybe T.Text -> T.Text -> Int -> Int -> ClientM (Either String  [Entry])
getChainsRange  :: Maybe T.Text -> Int -> Int -> ClientM (Either String  [Chain])
(     getEntriesRange
 :<|> getChainsRange) = client factomExtendedAPI

--------------------------------------------------------------------------------

getUser :: Maybe T.Text -> ClientM User
getUserChains :: Maybe T.Text -> Maybe Int -> Maybe Int -> Maybe T.Text -> Maybe T.Text -> ClientM (Either String [Chain])
createChain :: Maybe T.Text -> Maybe T.Text -> [(T.Text, T.Text)] -> ClientM (Either String Chain)
getChainById :: Maybe T.Text -> T.Text -> ClientM (Either String Chain)
getChainEntries :: Maybe T.Text -> T.Text -> Maybe Int -> Maybe Int -> Maybe T.Text -> Maybe T.Text -> ClientM (Either String  [Entry])
getChainEntryFirst :: Maybe T.Text -> T.Text -> ClientM (Either String Entry)
getChainEntryLast  :: Maybe T.Text -> T.Text -> ClientM (Either String Entry)
searchChainsByExternalIds :: Maybe T.Text -> Maybe Int -> Maybe Int -> Maybe T.Text -> Maybe T.Text -> [T.Text] -> ClientM (Either String [Chain])
searchEntriesChainByExternalIds :: Maybe T.Text -> T.Text -> Maybe Int -> Maybe Int -> Maybe T.Text -> Maybe T.Text -> [T.Text] -> ClientM (Either String [Entry])
(      getUser
  :<|> getUserChains
  :<|> createChain
  :<|> getChainById
  :<|> getChainEntries
  :<|> getChainEntryFirst
  :<|> getChainEntryLast
  :<|> searchChainsByExternalIds
  :<|> searchEntriesChainByExternalIds
 ) = client factomAPIMinimal

--------------------------------------------------------------------------------

getUser' :: T.Text
         -> Maybe T.Text
         -> IO (Either ServantError User)
getUser' endpoint token = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getUser token) env         -- token: (Just "my-awesome-token")
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

getUserChains' :: T.Text
               -> Maybe T.Text
               -> Maybe Int
               -> Maybe Int
               -> Maybe T.Text
               -> Maybe T.Text
               -> IO (Either ServantError (Either String [Chain]))
getUserChains' endpoint token start limit status sort = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getUserChains token start limit status sort) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

createChain' :: T.Text
             -> Maybe T.Text
             -> Maybe T.Text
             -> [(T.Text, T.Text)]
             -> IO (Either ServantError (Either String Chain))
createChain' endpoint token callback datas= do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  -- datas = [] -- add something if needed to data
  runClientM (createChain token callback datas ) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

getChainById' :: T.Text
              -> Maybe T.Text
              -> T.Text
              -> IO (Either ServantError (Either String Chain))
getChainById' endpoint token chainid = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
      datas = [] -- prepare needed data
  runClientM (getChainById token chainid ) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

getChainEntries' :: T.Text
                 -> Maybe T.Text
                 -> T.Text
                 -> Maybe Int
                 -> Maybe Int
                 -> Maybe T.Text
                 -> Maybe T.Text
                 -> IO (Either ServantError (Either String [Entry]))
getChainEntries' endpoint token chainid start limit status sort = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getChainEntries token chainid start limit status sort) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

getChainEntryFirst' :: T.Text
                    -> Maybe T.Text
                    -> T.Text
                    -> IO (Either ServantError (Either String Entry))
getChainEntryFirst' endpoint token chainid = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getChainEntryFirst token chainid) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

getChainEntryLast' :: T.Text
                   -> Maybe T.Text
                   -> T.Text
                   -> IO (Either ServantError (Either String Entry))
getChainEntryLast' endpoint token chainid = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getChainEntryFirst token chainid) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

searchChainsByExternalIds' :: T.Text
                           -> Maybe T.Text
                           -> [T.Text]
                           -> Maybe Int
                           -> Maybe Int
                           -> Maybe T.Text
                           -> Maybe T.Text
                           -> IO (Either ServantError (Either String [Chain]))
searchChainsByExternalIds' endpoint token datas start limit status sort = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (searchChainsByExternalIds token start limit status sort datas) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

searchEntriesChainByExternalIds' :: T.Text
                                 -> Maybe T.Text
                                 -> T.Text
                                 -> [T.Text]
                                 -> Maybe Int
                                 -> Maybe Int
                                 -> Maybe T.Text
                                 -> Maybe T.Text
                                 -> IO (Either ServantError (Either String [Entry]))
searchEntriesChainByExternalIds' endpoint token chainid datas start limit status sort = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (searchEntriesChainByExternalIds token chainid start limit status sort datas) env
  where
    host = (BaseUrl Https (T.unpack endpoint) 443 "")

--------------------------------------------------------------------------------

getEntriesRange' = undefined

getChainsRange' = undefined
