{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
module WishlistSimple where

import Prelude hiding (log)

import Control.Monad.Reader
import Data.IORef
import Data.Proxy
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp

import Wishlist.Utils
import Wishlist.Types

-- part #1: the service API
type API =
      "wishes" :> Get '[JSON] Wishlist
 :<|> "wishes" :> Capture "shop" Shop :> Get '[JSON] Wishlist
 :<|> "wishes" :> ReqBody '[JSON] Wish :> Post '[JSON] ()

-- part #2: a service for the above API
type Store = IORef Wishlist
service :: Service Store API
service = getAllWishes :<|> getShopWishes :<|> postNewWish

getAllWishes :: Controller Store Wishlist
getAllWishes = log "getAllWishes" $ do
  store <- ask
  liftIO (readIORef store)

getShopWishes :: Shop -> Controller Store Wishlist
getShopWishes shop = log "getShopWishes" $ do
  wishlist <- getAllWishes
  return $ filter (\wish -> getShop wish == shop) wishlist

postNewWish :: Wish -> Controller Store ()
postNewWish wish = logWith "postNewWish " wish $ do
  store <- ask
  liftIO $ do
    wishlist <- readIORef store
    writeIORef store (wish:wishlist)

-- part #3: run the server
main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  store <- newIORef []
  run 8080 $ serve (Proxy :: Proxy API) (enter (toHandler store) service)
