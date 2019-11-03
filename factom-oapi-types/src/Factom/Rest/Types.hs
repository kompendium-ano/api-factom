{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module Factom.Rest.Types
  ( ApiErrorResponse (..)
  , ApiSuccessResponse (..)
  , ApiSuccessResponsePagination (..)
  ) where

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

-- |
data ApiErrorResponse = ApiErrorResponse
  { apiErrorResponseCode   :: Int  -- ^
  , apiErrorResponseError  :: Text -- ^
  , apiErrorResponseResult :: Bool -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ApiErrorResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiErrorResponse")
instance ToJSON ApiErrorResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiErrorResponse")

-- |
data ApiSuccessResponse = ApiSuccessResponse
  { apiSuccessResponseResult :: Value -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ApiSuccessResponse where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiSuccessResponse")
instance ToJSON ApiSuccessResponse where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiSuccessResponse")

-- |
data ApiSuccessResponsePagination = ApiSuccessResponsePagination
  { apiSuccessResponsePaginationLimit  :: Int -- ^
  , apiSuccessResponsePaginationResult :: Value -- ^
  , apiSuccessResponsePaginationStart  :: Int -- ^
  , apiSuccessResponsePaginationTotal  :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ApiSuccessResponsePagination where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "apiSuccessResponsePagination")
instance ToJSON ApiSuccessResponsePagination where
  toJSON = genericToJSON (removeFieldLabelPrefix False "apiSuccessResponsePagination")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
