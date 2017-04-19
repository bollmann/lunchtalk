{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Wishlist.Types where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.IORef
import Data.Map (Map)
import qualified Data.Text as Text
import GHC.Generics
import Servant
type Wishlist = [Wish]

data Wish = Wish
  { getName :: String
  , getShop :: Shop
  } deriving (Show, Read, Generic)

data Shop = Amazon | Otto | Zalando
  deriving (Eq, Show, Read, Generic)

newtype Tenant = Tenant String
  deriving (Eq, Ord, Show)

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

type Service store api = ServerT api (Controller store)
type Controller store = ReaderT store (ExceptT ServantErr IO)

toHandler :: forall store. store -> (Controller store :~> Handler)
toHandler st = NT controllerToHandler where
  controllerToHandler :: Controller store a -> Handler a
  controllerToHandler m = Handler (runReaderT m st)
