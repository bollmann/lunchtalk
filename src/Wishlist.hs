{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE NoImplicitPrelude       #-}
module Wishlist where

import Prelude hiding (log)

import Control.Monad.Reader
import Data.IORef
import Data.Proxy
import Servant.API
import Servant.Server
import Network.Wai.Handler.Warp

import Wishlist.Types.Simple
import Wishlist.Utils

-- part #1: the service API
type API = ???

-- part #2: a service for the above API
service :: Service API
service = error "???"

-- part #3: run the server
main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  store <- newIORef []
  run 8080 $ serve (Proxy :: Proxy API) (enter (toHandler store) service)
