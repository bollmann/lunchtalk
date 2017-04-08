{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module More where

import Data.Text
import Servant

import Wishlist
import Wishlist.Types

{- some more cool servant features: -}

-- type-safe links:
allWishesLink :: Text
allWishesLink = toUrlPiece $ safeLink api allwishes
  where
    api = Proxy :: Proxy API
    allwishes = Proxy :: Proxy (
      "wishes" :> Get '[JSON] (Headers '[Header "Wish-Count" Int] Wishlist))

-- type-safe links:
allWishesLink :: Text
allWishesLink = toUrlPiece $ safeLink api allwishes
  where
    api = Proxy :: Proxy API
    allwishes = Proxy :: Proxy ("wishes" :> Get '[JSON] Wishlist)
