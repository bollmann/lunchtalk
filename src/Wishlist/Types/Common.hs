{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Wishlist.Types.Common where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
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

instance FromJSON Shop
instance ToJSON Shop

instance FromJSON Wish where
  parseJSON (Object v) = Wish <$> v .: "name" <*> v .: "shop"
  parseJSON _          = empty

instance ToJSON Wish where
  toJSON (Wish name shop) = object [ "name" .= name, "shop" .= shop ]

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

type Service' store api = ServerT api (Controller' store)
type Controller' store  = ReaderT store (ExceptT ServantErr IO)

toHandler :: forall store. store -> Controller' store :~> Handler
toHandler st = NT controllerToHandler where
  controllerToHandler :: Controller' store a -> Handler a
  controllerToHandler m = Handler (runReaderT m st)
