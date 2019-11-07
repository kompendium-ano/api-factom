{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent             (forkIO, threadDelay)
import           Control.Concurrent.STM.TVar
import           Control.Monad                  (forever)
import           Data.Char
import           Data.Maybe
import qualified Data.Text                      as T
import           Data.Time.Clock
import           Elm
import           Servant.Elm
import           System.Environment

import qualified Factom.Rest.Client.Api         as FactomApi
import qualified Factom.Rest.Client.Types.Chain as FactomApi
import qualified Factom.Rest.Client.Types.Entry as FactomApi
import qualified Factom.Rest.Client.Types.User  as FactomApi
import qualified Factom.Rest.Client.Types.User  as FactomApi
import qualified Factom.Rest.Client.Utils       as FactomApi

--------------------------------------------------------------------------------

main :: IO ()
main =
  do
    let delay = 60000  -- 1 request per minute available
    a       <- getArgs
    timeVar <- newTVarIO (delay*1000)
    case a of
      ["-ge"]    -> do
        -- generate ELM API
        specsToDir [spec] "assets/generated/api/elm"
        return ()
      otherwise -> do
        putStrLn $ "Factom | Not tokenized API access available for version info only"
        putStrLn $ "Factom | Getting user chcains \n"

        mchainsE <- FactomApi.getUserChains'
                      "http://localhost:8081/v1"
                      (Just "1q3sre2we25ph")
                      Nothing
                      Nothing
                      Nothing
                      Nothing

        case mchainsE of
          Left err ->
            -- some error happend
            putStrLn $ show $ err
          Right mchains -> do
            -- we got the data proceed with showing entry hashes
            --let entHashes = map FactomApi.chainEntChainId $ map FactomApi.chainChainEntity mchains
            --putStrLn $ show $ entHashes
            return ()


spec :: Spec
spec =
  Spec ["FactomApi"]
       (  defElmImports
         : toElmTypeSource (Proxy :: Proxy FactomApi.User)
         : toElmTypeSource (Proxy :: Proxy FactomApi.Entry)
         : toElmTypeSource (Proxy :: Proxy FactomApi.Chain)
         : toElmDecoderSource (Proxy :: Proxy FactomApi.User)
         : toElmDecoderSourceWith (defaultOptions {fieldLabelModifier = withoutPrefix "ac"}) (Proxy :: Proxy FactomApi.Chain)
         : toElmDecoderSourceWith (defaultOptions {fieldLabelModifier = withoutPrefix "st"}) (Proxy :: Proxy FactomApi.Entry)
         : generateElmForAPI (Proxy :: Proxy FactomApi.FactomAPIMinimal))


initCap :: T.Text -> T.Text
initCap t =
  case T.uncons t of
    Nothing      -> t
    Just (c, cs) -> T.cons (Data.Char.toUpper c) cs

withPrefix :: T.Text -> T.Text -> T.Text
withPrefix prefix s = prefix <> initCap s

withoutPrefix :: T.Text -> T.Text -> T.Text
withoutPrefix prefix s =
  FactomApi.uncapFst' $ fromMaybe s $ T.stripPrefix prefix s
