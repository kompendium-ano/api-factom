{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Factom.Rest.Server.Api
  ( restAPIvCombined
  , serverCombined
  ) where

import           GHC.Generics
import           Jose.Jws
import qualified Network.Socket                      as NSocket
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Safe
import           Servant                             hiding (BadPassword,
                                                      NoSuchUser)
import           Servant.API
import           Servant.API.BasicAuth
import           Servant.API.Experimental.Auth       (AuthProtect)
import           Servant.Auth                        hiding (NoSuchUser)
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan ()
import           Servant.Multipart
import           Servant.Server                      hiding (BadPassword,
                                                      NoSuchUser)
import           Servant.Swagger

-- import local modules
import           Factom.Rest.Server.Config
import           Factom.Rest.Server.Types.Chain
import           Factom.Rest.Server.Types.User
import           Factom.Rest.Server.Types.Version

--------------------------------------------------------------------------------


type APIv1Protected =
       ""
    :> Get '[JSON] VersionResult

  :<|> "user"
    :> Get '[JSON] User

  :<|> "chains"
    :> Post '[JSON] ChainResult

  -- :<|>


type APIv1UnProtected =
  "ping"
   :> Get '[JSON] Bool

type APIv1 auths =
  "v1" :>
  (    (Auth auths UserCredBasic :> APIv1Protected) -- Protected
  :<|> APIv1UnProtected                             -- UnProtected
  )

type APIvCombined auths = APIv1 auths

realmProtectedV1 :: AppConfig
                 -> AuthResult UserCredBasic
                 -> Server APIv1Protected
realmProtectedV1 cc BadPassword = throwAll err401 { errHeaders = [("WWW-Authenticate", "bad password")]}
realmProtectedV1 cc NoSuchUser  = throwAll err401 { errHeaders = [("WWW-Authenticate", "no such user")]}
realmProtectedV1 cc Indefinite  = throwAll err401 { errHeaders = [("WWW-Authenticate", "wat?")]}
realmProtectedV1 cc (Authenticated u) =
       getVersion
  :<|> getUser
  :<|> createNewChain

realmUnProtectedV1 :: AppConfig
                   -> CookieSettings
                   -> JWTSettings
                   -> Server APIv1UnProtected
realmUnProtectedV1 cc cs jws =
       servicePing
-- :<|> some other function

serverV1 :: AppConfig
         -> CookieSettings
         -> JWTSettings
         -> Server (APIv1 auths)
serverV1 cc cs jwts =
       realmProtectedV1   cc
  :<|> realmUnProtectedV1 cc cs jwts

serverCombined :: AppConfig
               -> CookieSettings
               -> JWTSettings
               -> Server (APIvCombined auths)
serverCombined cc cs jwts =
       serverV1 cc cs jwts

restAPIv1 :: Proxy (APIv1 '[JWT])
restAPIv1 = Proxy

restAPIvCombined :: Proxy (APIvCombined '[JWT])
restAPIvCombined = Proxy

--------------------------------------------------------------------------------

getVersion = undefined
getUser = undefined
createNewChain = undefined
servicePing = undefined
