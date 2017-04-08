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
 :<|> "wishes" :> Capture "from" Shop :> Get '[JSON] Wishlist
 :<|> "new" :> ReqBody '[JSON] Wish :> Post '[JSON] Wishlist


-- part #2: a server for the above API
server :: Service API
server = getAllWishes :<|> getShopWishes :<|> postNewWish

getAllWishes :: Endpoint Store Wishlist
getAllWishes = log "getAllWishes" $ do
  store <- ask
  liftIO (readIORef store)

getShopWishes :: Shop -> Endpoint Store Wishlist
getShopWishes shop = log "getShopWishes" $ do
  wishlist <- getAllWishes
  return $ filter (\wish -> getShop wish == shop) wishlist

postNewWish :: Wish -> Endpoint Store Wishlist
postNewWish wish = logWith "postNewWish " wish $ do
  store <- ask
  liftIO $ do
    wishlist <- readIORef store
    writeIORef store (wish:wishlist)
    return (wish:wishlist)

-- part #3: run the server
main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  ref <- newIORef []
  run 8080 $ serve (Proxy :: Proxy API) (enter (endpointToHandler ref) server)
