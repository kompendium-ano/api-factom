module Factom.Rest.Client.Utils where

import qualified Data.Char  as Char
import           Data.List  (stripPrefix)
import qualified Data.Map   as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Text  as T

--------------------------------------------------------------------------------

uncapFst :: String -> String
uncapFst (head:tail) = Char.toLower head : tail
uncapFst []          = []

uncapFst' :: T.Text -> T.Text
uncapFst' s = T.concat [T.toLower . T.singleton $ T.head s, T.tail s]
