{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Wishlist where

import Wishlist.Types

import Control.Monad.Trans
import Control.Monad.Except
import Data.IORef
import Data.Text (Text)
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.Proxy
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp

-- part #1: the service API
type API =
      "wishes" :> Header "Tenant" Tenant :> Get '[JSON] Wishlist
 :<|> "wishes" :> Header "Tenant" Tenant :> Capture "shop" Shop
        :> Get '[JSON] Wishlist
 :<|> "new" :> Header "Tenant" Tenant :> ReqBody '[JSON] Wish
        :> Post '[JSON] Wishlist

type Store = IORef (Map Tenant Wishlist)

-- part #2: a server for the above API
server :: IORef (Map Tenant Wishlist) -> Server API
server ref = getAllWishes ref
  :<|> getShopWishes ref
  :<|> postNewWish ref

getAllWishes :: Store -> Maybe Tenant -> Handler Wishlist
getAllWishes _ Nothing = return []
getAllWishes store (Just tenant) = do
  tenants <- liftIO (readIORef store)
  case Map.lookup tenant tenants of
    Just wishlist -> return wishlist
    Nothing       -> return []

getShopWishes :: Store -> Maybe Tenant -> Shop -> Handler Wishlist
getShopWishes store maybeTenant shop = do
  wishlist <- getAllWishes store maybeTenant
  return $ filter (\wish -> getShop wish == shop) wishlist

postNewWish :: Store -> Maybe Tenant -> Wish -> Handler Wishlist
postNewWish _ Nothing _ =
  throwError err403 { errBody = "you must provide a tenant to add a wish!" }
postNewWish store (Just tenant) wish = do
  tenants <- liftIO (readIORef store)
  let tenants'  = Map.insertWith (++) tenant [wish] tenants
      wishlist' = fromJust $ Map.lookup tenant tenants'
  liftIO (writeIORef store tenants')
  return wishlist'

main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  ref <- newIORef Map.empty
  run 8080 $ serve (Proxy :: Proxy API) (server ref)

-- more cool features:

-- type-safe links:
allWishesLink :: Text
allWishesLink = toUrlPiece $ safeLink api allwishes
  where
    api = Proxy :: Proxy API
    allwishes = Proxy :: Proxy ("wishes" :> Get '[JSON] Wishlist)
