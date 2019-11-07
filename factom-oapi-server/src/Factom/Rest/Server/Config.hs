{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Factom.Rest.Server.Config
  ( AppConfig(..)
  , ConnectConfig(..)
  , MetaConfig(..)
  , defaultConnectConfig
  , readCfg
  , readEvalCfg
  , viewCfg
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as BS
import           Data.List
import           Data.List.Split
import           Data.Maybe            (catMaybes, fromMaybe)
import qualified Data.Text             as T
import           Data.Typeable
import           Data.UUID
import           Data.Word
import qualified Data.Yaml             as Y
import           GHC.Generics
import           System.Environment    (getEnv, lookupEnv)
import           System.IO.Unsafe

import           Common.Types.Db

-------------------------------------------------------------------------------

-- | Top level structure for storing config
--
data AppConfig =
  AppConfig
    { lcDatabase :: ConnectConfig
    , lcMeta     :: MetaConfig      -- ^ parameters related to meta information
    , lcHost     :: String          -- ^ system host link
    , lcApiHost  :: String          -- ^ API host link
    } deriving (Generic, ToJSON, Show)

instance FromJSON AppConfig where
  parseJSON = withObject "config" $ \o -> do
    db <- o .: "database"
    mt <- o .: "meta"
    hh <- o .: "host"
    ah <- o .: "host-api"
    return $ AppConfig db lg mt hh ah

instance FromJSON ConnectConfig where
  parseJSON = withObject "database" $ \o -> do
    hs <- o .: "host"
    pr <- o .: "port"
    dd <- o .: "db"
    us <- o .: "user"
    ps <- o .: "password"
    return $ ConnectConfig hs pr dd us psx

-- | Additional structure for specific meta values
--
data MetaConfig = MetaConfig
  { mcDelay   :: Int      -- ^ time delay between reimport for daemon mode
  , mcThreads :: Int      -- ^ number of threads we able to spawn
  } deriving (Generic, ToJSON, Show)

instance FromJSON MetaConfig where
  parseJSON = withObject "meta" $ \o -> do
    dl <- o .: "delay"
    th <- o .: "threads"
    return $ MetaConfig dl th

--------------------------------------------------------------------------------

viewCfg :: ConnectConfig -> String
viewCfg (ConnectConfig h p db u ps) =
  "Connection parameters:\n"
   ++ " server - " ++ h  ++ "\n"
   ++ " port   - " ++ p  ++ "\n"
   ++ " db     - " ++ db ++ "\n"
   ++ " user   - " ++ u  ++ "\n"

-- | Reading database config from specified file
--
readCfg :: String -> IO (Either Y.ParseException AppConfig)
readCfg path = Y.decodeFileEither path

-- | Reads config and check environment values
--   to override defaults
readEvalCfg :: String -> IO (Either String AppConfig)
readEvalCfg path =
  do
    content <- BS.readFile path
    --putStrLn $ BS.unpack content
    let parsedContent = Y.decodeEither' content :: Either Y.ParseException AppConfig
    case parsedContent of
      Left exc -> do
        print $ show $ exc
        return $ Left "Could not parse config file."
      Right cfg@(AppConfig (ConnectConfig h p d u ps) m hh ap) ->
           return $
             Right AppConfig { lcDatabase     = connConfig
                                , lcMeta         = m
                                , lcHost         = hh
                                , lcApiHost      = ap
                                }
               where
                 connConfig =
                   ConnectConfig { host     = evalCfgField h
                                 , port     = evalCfgField p
                                 , dbs      = evalCfgField d
                                 , user     = evalCfgField u
                                 , pass     = evalCfgField ps
                                 }

-- | Evaluates each field and take care of environment overloading
evalCfgField :: String -> String
evalCfgField s =
    case isInfixOf "_env" s of
      True ->
        fromMaybe lvar $ unsafePerformIO $ lookupEnv evar
          where rs   = tail $ splitOn ":" s
                evar = head rs
                lvar = last rs
      False -> s

-- | Reading import config from specified file
--   this file have information about table name, field names,
--   and other useful information that need to be stored in DB
--   since some of files doesn't have metadata we need to provide additional
--   information for parsing
readImportCfg :: String -> IO (Either Y.ParseException ConnectConfig)
readImportCfg path = Y.decodeFileEither path

-- | This is default parameters that
--   are in use if config wasn't supplied
defaultConnectConfig = ConnectConfig {
    host     = "db"
  , port     = "5432"
  , dbs      = "factom-oapi"
  , user     = "factom"
  , pass     = "factom-drum"
 }
