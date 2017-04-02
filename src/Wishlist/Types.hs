{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Wishlist.Types where

import Control.Monad.Except
import Control.Monad.State
import Data.Aeson
import qualified Data.Text as Text
import GHC.Generics
import Servant
type Wishlist = [Wish]

data Wish = Wish
  { getPriority :: Int
  , getName     :: String
  , getShop     :: Shop
  } deriving (Show, Read, Generic)

data Shop = Amazon | Otto | Zalando
  deriving (Eq, Show, Read, Generic)

newtype Tenant = Tenant String deriving (Eq, Ord)

instance FromJSON Shop
instance ToJSON Shop
instance FromJSON Wish
instance ToJSON Wish

instance FromHttpApiData Shop where
  parseUrlPiece value
    | value == "amazon"  = pure Amazon
    | value == "otto"    = pure Otto
    | value == "zalando" = pure Zalando
  parseUrlPiece value    = fail $
    "parseUrlPiece: could not parse `Shop' value '" ++ Text.unpack value ++ "'"

instance ToHttpApiData Shop where
  toUrlPiece Amazon  = "amazon"
  toUrlPiece Otto    = "otto"
  toUrlPiece Zalando = "zalando"

instance FromHttpApiData Tenant where
  parseUrlPiece = fmap Tenant . parseUrlPiece

instance ToHttpApiData Tenant where
  toUrlPiece (Tenant tenant) = toUrlPiece tenant

type Endpoint = ExceptT ServantErr (StateT Wishlist IO)

endpointToHandler :: forall a. Endpoint a -> Handler a
endpointToHandler m = Handler . ExceptT $ evalStateT (runExceptT m) []
