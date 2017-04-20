module Wishlist.Clients.MultiTenant where

import Data.Proxy
import Network.HTTP.Client hiding (Proxy)
import Servant.API
import Servant.Client

import WishlistMT
import Wishlist.Types.MultiTenant

-- part #4: client functions for the wishlist service API
allWishes :: Maybe Tenant -> ClientM RichWishlist
shopWishes :: Maybe Tenant -> Shop -> ClientM RichWishlist
addWish :: Maybe Tenant -> Wish -> ClientM ()
allWishes :<|> shopWishes :<|> addWish = client (Proxy :: Proxy API)

exec :: ClientM a -> IO (Either ServantError a)
exec query = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8080 "")
  runClientM query clientEnv
