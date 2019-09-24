{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Factom.Rest.Server.API
  ( ServerConfig(..)
  , FactomOpenBackend
  , runFactomOpenServer
  , FactomOpenAPI
  ) where

import           Control.Monad.Except      (ExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans
import           Data.Aeson                (Value)
import           Data.Coerce               (coerce)
import           Data.Function             ((&))
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Proxy                (Proxy (..))
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           GHC.Exts                  (IsString (..))
import           GHC.Generics              (Generic)
import           Network.HTTP.Client       (Manager, defaultManagerSettings,
                                            newManager)
import           Network.HTTP.Types.Method (methodOptions)
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant
import           Servant.API
import           Servant.API.ContentTypes
import           Servant.API.Verbs         (StdMethod (..), Verb)
import           Servant.Client            (Scheme (Http), client)
import           Servant.Server
import           Servant.Swagger
import           Web.FormUrlEncoded
import           Web.HttpApiData
-- import           Servant.Common.BaseUrl    (BaseUrl (..))
import           Web.HttpApiData

-- import local types
import           Factom.Rest.Types

-----------------------------------------------------------------------------------

data FormChainsChainIdEntriesSearchPost = FormChainsChainIdEntriesSearchPost
  { chainsChainIdEntriesSearchPostExtIds :: [Text]
  } deriving (Show, Eq, Generic)

-- instance FormUrlEncoded FormChainsChainIdEntriesSearchPost where
--   fromFormUrlEncoded inputs =
--     FormChainsChainIdEntriesSearchPost
--       <$> lookupEither "extIds" inputs

-- instance FormUrlEncoded FormChainsChainIdEntriesSearchPost where
--   toFormUrlEncoded value =
--     [ ("extIds", toQueryParam $ chainsChainIdEntriesSearchPostExtIds value)
--     ]

data FormChainsPost = FormChainsPost
  { chainsPostExtIds  :: [Text]
  , chainsPostContent :: Text
  } deriving (Show, Eq, Generic)

-- instance FormUrlEncoded FormChainsPost where
--   fromFormUrlEncoded inputs = FormChainsPost <$> lookupEither "extIds" inputs <*> lookupEither "content" inputs

-- instance FormUrlEncoded FormChainsPost where
--   toFormUrlEncoded value =
--     [ ("extIds", toQueryParam $ chainsPostExtIds value)
--     , ("content", toQueryParam $ chainsPostContent value)
--     ]

data FormChainsSearchPost = FormChainsSearchPost
  { chainsSearchPostExtIds :: [Text]
  } deriving (Show, Eq, Generic)

-- instance FormUrlEncoded FormChainsSearchPost where
--   fromFormUrlEncoded inputs = FormChainsSearchPost <$> lookupEither "extIds" inputs

-- instance FormUrlEncoded FormChainsSearchPost where
--   toFormUrlEncoded value =
--     [ ("extIds", toQueryParam $ chainsSearchPostExtIds value)
--     ]

data FormEntriesPost = FormEntriesPost
  { entriesPostChainId :: Text
  , entriesPostExtIds  :: [Text]
  , entriesPostContent :: Text
  } deriving (Show, Eq, Generic)

-- instance FormUrlEncoded FormEntriesPost where
--   fromFormUrlEncoded inputs =
--     FormEntriesPost
--     <$> lookupEither "chainId" inputs
--     <*> lookupEither "extIds"  inputs
--     <*> lookupEither "content" inputs

-- instance FormUrlEncoded FormEntriesPost where
--   toFormUrlEncoded value =
--     [ ("chainId", toQueryParam $ entriesPostChainId value)
--     , ("extIds" , toQueryParam $ entriesPostExtIds value)
--     , ("content", toQueryParam $ entriesPostContent value)
--     ]

data FormFactomdMethodPost = FormFactomdMethodPost
  { factomdMethodPostParams :: Text
  } deriving (Show, Eq, Generic)

-- instance FormUrlEncoded FormFactomdMethodPost where
--   fromFormUrlEncoded inputs = FormFactomdMethodPost <$> lookupEither "params" inputs

-- instance FormUrlEncoded FormFactomdMethodPost where
--   toFormUrlEncoded value =
--     [ ("params", toQueryParam $ factomdMethodPostParams value)
--     ]

-- For the form data code generation.
lookupEither :: FromHttpApiData b => Text -> [(Text, Text)] -> Either String b
lookupEither key assocs =
  case lookup key assocs of
    Nothing -> Left $ "Could not find parameter " <> (T.unpack key) <> " in form data"
    Just value ->
      case parseQueryParam value of
        Left result  -> Left $ T.unpack result
        Right result -> Right $ result

--------------------------------------------------------------------------------

-- | Server description with routes to specific methods
--
type FactomOpenAPI
    =    ""
      :> Verb 'GET 200 '[JSON] ApiSuccessResponse -- 'rootGet' route

    :<|> "user"
      :> Verb 'GET 200 '[JSON] ApiSuccessResponse -- 'userGet' route

    :<|>  "chains"
      :> Capture "chainId" Text
      :> "entries"
      :> "first"
      :> Verb 'GET 200 '[JSON] ApiSuccessResponse -- 'chainsChainIdEntriesFirstGet' route

    :<|> "chains"
      :> Capture "chainId" Text
      :> "entries"
      :> QueryParam "start" Int
      :> QueryParam "limit" Int
      :> QueryParam "status" Text
      :> QueryParam "sort" Text
      :> Verb 'GET 200 '[JSON] ApiSuccessResponsePagination -- 'chainsChainIdEntriesGet' route

    :<|> "chains"
      :> Capture "chainId" Text
      :> "entries"
      :> "last"
      :> Verb 'GET 200 '[JSON] ApiSuccessResponse -- 'chainsChainIdEntriesLastGet' route

    :<|> "chains"
      :> Capture "chainId" Text
      :> "entries"
      :> "search"
      :> QueryParam "start" Int
      :> QueryParam "limit" Int
      :> QueryParam "status" Text
      :> QueryParam "sort" Text
      :> ReqBody '[FormUrlEncoded] FormChainsChainIdEntriesSearchPost
      :> Verb 'POST 200 '[JSON] ApiSuccessResponsePagination -- 'chainsChainIdEntriesSearchPost' route

    :<|> "chains"
      :> Capture "chainId" Text
      :> Verb 'GET 200 '[JSON] ApiSuccessResponse -- 'chainsChainIdGet' route

    :<|> "chains"
      :> QueryParam "start" Int
      :> QueryParam "limit" Int
      :> QueryParam "status" Text
      :> QueryParam "sort" Text
      :> Verb 'GET 200 '[JSON] ApiSuccessResponsePagination -- 'chainsGet' route

    :<|> "chains"
      :> ReqBody '[FormUrlEncoded] FormChainsPost
      :> Verb 'POST 200 '[JSON] ApiSuccessResponse -- 'chainsPost' route

    :<|> "chains"
      :> "search"
      :> QueryParam "start" Int
      :> QueryParam "limit" Int
      :> QueryParam "status" Text
      :> QueryParam "sort" Text
      :> ReqBody '[FormUrlEncoded] FormChainsSearchPost
      :> Verb 'POST 200 '[JSON] ApiSuccessResponse -- 'chainsSearchPost' route

    :<|> "entries"
      :> Capture "entryHash" Text
      :> Verb 'GET 200 '[JSON] ApiSuccessResponse -- 'entriesEntryHashGet' route

    :<|> "entries"
      :> ReqBody '[FormUrlEncoded] FormEntriesPost
      :> Verb 'POST 200 '[JSON] ApiSuccessResponse -- 'entriesPost' route

    :<|> "factomd"
      :> Capture "method" Text
      :> ReqBody '[FormUrlEncoded] FormFactomdMethodPost
      :> Verb 'POST 200 '[JSON] () -- 'factomdMethodPost' route

-------------------------------------------------------------------------------------------------

-- | Server or client configuration, specifying the host and port to query or serve on.
data ServerConfig =
  ServerConfig
    { configHost :: String  -- ^ Hostname to serve on, e.g. "127.0.0.1"
    , configPort :: Int      -- ^ Port to serve on, e.g. 8080
    } deriving (Eq, Ord, Show, Read)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a =
  QueryList
    { fromQueryList :: [a]
    } deriving (Functor, Applicative, Monad, Foldable, Traversable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated  -- ^ CSV format for multiple parameters.
  | SpaceSeparated  -- ^ Also called "SSV"
  | TabSeparated    -- ^ Also called "TSV"
  | PipeSeparated   -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a =>
  FromHttpApiData (QueryList 'CommaSeparated a) where
    parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a =>
  FromHttpApiData (QueryList 'TabSeparated a) where
    parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a =>
  FromHttpApiData (QueryList 'SpaceSeparated a) where
    parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a =>
  FromHttpApiData (QueryList 'PipeSeparated a) where
    parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a =>
  FromHttpApiData (QueryList 'MultiParamArray a) where
    parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList

-- | Backend for FactomOpen.
-- The backend can be used both for the client and the server. The client generated from the FactomOpen Swagger spec
-- is a backend that executes actions by sending HTTP requests (see @createFactomOpenClient@). Alternatively, provided
-- a backend, the API can be served using @runFactomOpenServer@.
data FactomOpenBackend m = FactomOpenBackend
  { chainsChainIdEntriesFirstGet :: Text -> m ApiSuccessResponse{- ^ Returns first entry of Factom chain -}
  , chainsChainIdEntriesGet :: Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> m ApiSuccessResponsePagination{- ^ Returns entries of Factom chain -}
  , chainsChainIdEntriesLastGet :: Text -> m ApiSuccessResponse{- ^ Returns last entry of Factom chain -}
  , chainsChainIdEntriesSearchPost :: Text -> Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> FormChainsChainIdEntriesSearchPost -> m ApiSuccessResponsePagination{- ^ Search entries into Factom chain by external id(s) -}
  , chainsChainIdGet :: Text -> m ApiSuccessResponse{- ^ Returns Factom chain by Chain ID -}
  , chainsGet :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> m ApiSuccessResponsePagination{- ^ Returns all user's chains -}
  , chainsPost :: FormChainsPost -> m ApiSuccessResponse{- ^ Creates chain on the Factom blockchain -}
  , chainsSearchPost :: Maybe Int -> Maybe Int -> Maybe Text -> Maybe Text -> FormChainsSearchPost -> m ApiSuccessResponse{- ^ Search user's chains by external id(s) -}
  , entriesEntryHashGet :: Text -> m ApiSuccessResponse{- ^ Returns Factom entry by EntryHash -}
  , entriesPost :: FormEntriesPost -> m ApiSuccessResponse{- ^ Creates entry on the Factom blockchain -}
  , factomdMethodPost :: Text -> FormFactomdMethodPost -> m (){- ^ Sends direct request to factomd API -}
  , rootGet :: m ApiSuccessResponse{- ^ Get API version -}
  , userGet :: m ApiSuccessResponse{- ^ Get API user info -}
  }

-- | Run the FactomOpen server at the provided host and port.
runFactomOpenServer :: MonadIO m => ServerConfig -> FactomOpenBackend Handler -> m ()
runFactomOpenServer ServerConfig{..} backend =
  liftIO $
    Warp.runSettings warpSettings
      $ serve (Proxy :: Proxy FactomOpenAPI) (serverFromBackend backend)
  where
    warpSettings = Warp.defaultSettings & Warp.setPort configPort & Warp.setHost (fromString configHost)
    serverFromBackend FactomOpenBackend {..} =
      ( coerce chainsChainIdEntriesFirstGet
       :<|> coerce chainsChainIdEntriesGet
       :<|> coerce chainsChainIdEntriesLastGet
       :<|> coerce chainsChainIdEntriesSearchPost
       :<|> coerce chainsChainIdGet
       :<|> coerce chainsGet
       :<|> coerce chainsPost
       :<|> coerce chainsSearchPost
       :<|> coerce entriesEntryHashGet
       :<|> coerce entriesPost
       :<|> coerce factomdMethodPost
       :<|> coerce rootGet
       :<|> coerce userGet)
