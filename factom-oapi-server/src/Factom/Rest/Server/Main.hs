{-# LANGUAGE OverloadedStrings #-}

module Factom.Rest.Server.Main where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception                    as Exception
import           Control.Lens
import           Control.Monad                        (forever)
import           Control.Monad.IO.Class
import           Data.Aeson                           hiding (Options)
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Char8                as BSC
import qualified Data.ByteString.Lazy                 as BSL
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Data.Time
import qualified Database.PostgreSQL.Simple           as PSQL
import           Network.Wai
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Options.Applicative
import           Servant
import           Servant.API
import           Servant.API.BasicAuth
import           Servant.Auth
import           Servant.Auth.Server
import           System.Directory
import           System.Environment
import           System.IO
import qualified System.IO                            as IO
import           Text.Read

import           Factom.Rest.Server.Api

-----------------------------------------------------------------------------

data Options =
  Options
    { opPort   :: Maybe String
    , opConfig :: Maybe String
    , opMode   :: Bool
    }

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption
      ( long    "port"
     <> short   'p'
     <> metavar "PORT"
     <> help    "port to run server" ))
  <*> optional (strOption
      ( long    "config"
     <> short   'c'
     <> metavar "FILE"
     <> help    "Path to configuration file" ))
  <*> flag False True
      ( long "development"
     <> short 'd'
     <> long "Switch, to run in development mode (by default = false)")

-- | Description of the utility.
optionsDesc :: InfoMod Options -- ^ parser description
optionsDesc = fullDesc <> headerDesc
  where headerDesc = Options.Applicative.header "Plydis API service"

-- | Parser of the command-line options.
parser :: ParserInfo Options
parser = info (helper <*> optionsParser) optionsDesc

-------------------------------------------------------------------------------

corsPolicy :: Middleware
corsPolicy =
  cors
    ( const $
        Just $
          simpleCorsResourcePolicy
           { corsMethods        = ["OPTIONS", "GET", "POST", "PUT", "PATCH", "DELETE"]
           , corsRequestHeaders = [ "Authorization"
                                  , "Content-Type"
                                  , "X-Requested-With"
                                  , "Access-Control-Request-Headers"
                                  , "Access-Control-Allow-Origin"
                                  , "Access-Control-Allow-Methods"
                                  ]
           }
    )

--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser parser
  case opConfig opts of
    Nothing  -> do
      putStrLn $ "Provide configuration file with -c key!"
      return $ ()
    Just cfgFile -> do
      cfgE <- readEvalCfg cfgFile
      case cfgE of
        Left  err -> do
          print err
          return $ ()

        Right systemConfig -> do

          -- 1. check if signkey generated
          -- 2. generate the key for signing tokens and kept it
          -- 3. in 100% cases we provide production key along with executable
          isKeyExists <- doesFileExist "config/plydis-sign-key"
          signingKey  <-
            case isKeyExists of
              False -> do
                newKey <- generateKey
                let encKey = encode $ newKey
                BSL.writeFile "config/plydis-sign-key" encKey
                putStrLn  $ "|INFO | plydis-api | New signing key generated"
                return $ newKey
              True  -> do
                f   <- BSL.readFile "config/plydis-sign-key"
                let key = decode $ f
                putStrLn  $ "|INFO | plydis-api | Re-Using provided signing key"
                return $ fromJust key

          -- Adding some configurations. All authentications require CookieSettings to
          -- be in the context.
          let jwtCfg = defaultJWTSettings signingKey
              cfg    = defaultCookieSettings :. jwtCfg :. EmptyContext


          case opMode $ opts of
            False -> do
              putStrLn $ "|INFO | factom-open-api-server | Running production"
              run port $
                logStdoutDev $
                  serveWithContext restAPIvCombined cfg (serverCombined systemConfig defaultCookieSettings jwtCfg)

            True  -> do
              putStrLn $ "|INFO | factom-open-api | Running local development"
              run port $
                logStdoutDev $
                  corsPolicy $
                    serveWithContext restAPIvCombined cfg (serverCombined systemConfig defaultCookieSettings jwtCfg)
