{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
module Wishlist.Types.Simple
  ( module Wishlist.Types.Simple
  , module Wishlist.Types.Common
  ) where

import Data.IORef
import Servant

import Wishlist.Types.Common

type SimpleStore = IORef Wishlist
type Service api = ServerT api Controller
type Controller = Controller' SimpleStore
