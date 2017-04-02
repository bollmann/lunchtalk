{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Wishlist where

import Wishlist.Types

import Control.Monad.Trans
import Data.IORef
import Data.Text (Text)
import Data.Proxy
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp

-- part #1: the service API
type API =
      "wishes" :> Get '[JSON] Wishlist
 :<|> "wishes" :> Capture "from" Shop :> Get '[JSON] Wishlist
 :<|> "new" :> ReqBody '[JSON] Wish :> Post '[JSON] Wishlist

type Store = IORef Wishlist

-- part #2: a server for the above API
server :: Store -> Server API
server ref = getAllWishes ref
  :<|> getShopWishes ref
  :<|> postNewWish ref

getAllWishes :: Store -> Handler Wishlist
getAllWishes store = liftIO (readIORef store)

getShopWishes :: Store -> Shop -> Handler Wishlist
getShopWishes store shop = do
  wishlist <- getAllWishes store
  return $ filter (\wish -> getShop wish == shop) wishlist

postNewWish :: Store -> Wish -> Handler Wishlist
postNewWish store wish = do
  wishlist <- liftIO (readIORef store)

  liftIO (writeIORef store (wish:wishlist))
  return (wish:wishlist)

main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  ref <- newIORef []
  run 8080 $ serve (Proxy :: Proxy API) (server ref)

-- more cool features:

-- type-safe links:
allWishesLink :: Text
allWishesLink = toUrlPiece $ safeLink api allwishes
  where
    api = Proxy :: Proxy API
    allwishes = Proxy :: Proxy ("wishes" :> Get '[JSON] Wishlist)
