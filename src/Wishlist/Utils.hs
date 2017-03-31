module Wishlist.Utils where

import Network.HTTP.Client
import Servant.Client

exec :: ClientM a -> IO (Either ServantError a)
exec query = do
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8080 "")
  runClientM query clientEnv
