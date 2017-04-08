module Wishlist.Simple.Client where

import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Servant.API
import Servant.Client

import WishlistSimple
import Wishlist.Types

-- part #4: client functions for the wishlist service API
allWishes  :: ClientM Wishlist
shopWishes :: Shop -> ClientM Wishlist
addWish    :: Wish -> ClientM Wishlist

allWishes :<|> shopWishes :<|> addWish = client api
  where api = Proxy :: Proxy API

run :: ClientM a -> IO (Either ServantError a)
run query = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8080 "")
  runClientM query clientEnv
