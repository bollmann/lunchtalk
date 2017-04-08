{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
module Wishlist where

import Prelude hiding (log)

import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Except
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import Data.Proxy
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp

import Wishlist.Types
import Wishlist.Utils

-- part #1: the service API
type API =
      "wishes" :> Header "Tenant" Tenant
        :> Get '[JSON] (Headers '[Header "Wish-Count" Int] Wishlist)
 :<|> "wishes" :> Header "Tenant" Tenant :> Capture "shop" Shop
        :> Get '[JSON] (Headers '[Header "Wish-Count" Int] Wishlist)
 :<|> "new" :> Header "Tenant" Tenant :> ReqBody '[JSON] Wish
        :> Post '[JSON] Wishlist

-- part #2: a server for the above API
server :: TenantService API
server = getAllWishes :<|> getShopWishes :<|> postNewWish

getAllWishes
  :: Maybe Tenant
  -> Endpoint TenantStore (Headers '[Header "Wish-Count" Int] Wishlist)
getAllWishes maybeTenant =
  logWith "getAllWishes" maybeTenant (getAllWishes' maybeTenant)
  where
    getAllWishes' Nothing = do
      let noTenant = "you must provide a `Tenant' header to list all wishes!\n"
      throwError err400 { errBody = noTenant }
    getAllWishes' (Just tenant) = do
      store   <- ask
      tenants <- liftIO (readIORef store)
      case Map.lookup tenant tenants of
        Just wishlist -> return $ addHeader (length wishlist) wishlist
        Nothing       -> return $ addHeader 0 []

getShopWishes
  :: Maybe Tenant
  -> Shop
  -> Endpoint TenantStore (Headers '[Header "Wish-Count" Int] Wishlist)
getShopWishes maybeTenant shop = logWith "getShopWishes" shop $ do
  wishlist <- getAllWishes maybeTenant
  let shopWishes = filter (\wish -> getShop wish == shop) (getResponse wishlist)
  return $ addHeader (length shopWishes) shopWishes

postNewWish :: Maybe Tenant -> Wish -> Endpoint TenantStore Wishlist
postNewWish maybeTenant wish =
  logWith "postNewWish" maybeTenant (postWish maybeTenant)
  where
    postWish Nothing = do
      let noTenant = "you must provide a `Tenant' header to add a wish!\n"
      throwError err400 { errBody = noTenant }
    postWish (Just tenant) = do
      store <- ask
      tenants <- liftIO (readIORef store)
      let tenants'  = Map.insertWith (++) tenant [wish] tenants
          wishlist' = fromJust $ Map.lookup tenant tenants'
      liftIO (writeIORef store tenants')
      return wishlist'

-- part #3: running the server
main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  ref <- newIORef Map.empty
  run 8080 $ serve (Proxy :: Proxy API) (enter (endpointToHandler ref) server)
