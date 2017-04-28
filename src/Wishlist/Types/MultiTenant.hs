{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Wishlist.Types.MultiTenant
  ( module Wishlist.Types.MultiTenant
  , module Wishlist.Types.Common
  ) where

import Data.IORef
import Data.Map (Map)
import Servant

import Wishlist.Types.Common

newtype Tenant = Tenant String
  deriving (Eq, Ord, Show)

instance FromHttpApiData Tenant where
  parseUrlPiece = fmap Tenant . parseUrlPiece

instance ToHttpApiData Tenant where
  toUrlPiece (Tenant tenant) = toUrlPiece tenant

type RichWishlist = Headers '[Header "Wish-Count" Int] Wishlist

type MultiStore = IORef (Map Tenant Wishlist)
type Service api = Service' MultiStore api
type Controller = Controller' MultiStore
