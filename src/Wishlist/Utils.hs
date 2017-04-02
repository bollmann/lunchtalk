module Wishlist.Utils where

import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Servant.API
import Servant.Client

import Wishlist
import Wishlist.Types

-- part #3: client functions for the above API

allWishes :: ClientM Wishlist
shopWishes :: Shop -> ClientM Wishlist
addWish :: Wish -> ClientM Wishlist
allWishes :<|> shopWishes :<|> addWish = client (Proxy :: Proxy API)

exec :: ClientM a -> IO (Either ServantError a)
exec query = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8080 "")
  runClientM query clientEnv
