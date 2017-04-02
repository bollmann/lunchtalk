module Wishlist.Simple.Client where

import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Servant.API
import Servant.Client

import WishlistSimple
import Wishlist.Types

-- part #4: client functions for the wishlist service API
allWishes :: ClientM Wishlist
shopWishes :: Shop -> ClientM Wishlist
addWish :: Wish -> ClientM Wishlist
allWishes :<|> shopWishes :<|> addWish = client (Proxy :: Proxy API)

exec :: ClientM a -> IO (Either ServantError a)
exec query = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8080 "")
  runClientM query clientEnv
