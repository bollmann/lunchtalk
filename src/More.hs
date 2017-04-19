{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module More where

import Data.Text (Text)
import Servant

import WishlistSimple
import Wishlist.Types

{- some more cool servant features: -}

-- type-safe links:
wishlistLinks :: [Text]
wishlistLinks = map toUrlPiece
  [ allWishes
  , shopWishes Amazon
  , shopWishes Zalando
  , postWish
  ]
  where
    api = Proxy :: Proxy API
    allWishes = safeLink api
      (Proxy :: Proxy ("wishes" :> Get '[JSON] Wishlist))
    shopWishes = safeLink api
      (Proxy :: Proxy ("wishes" :> Capture "shop" Shop :> Get '[JSON] Wishlist))
    postWish = safeLink api
      (Proxy :: Proxy ("wishes" :> ReqBody '[JSON] Wish :> Post '[JSON] ()))
