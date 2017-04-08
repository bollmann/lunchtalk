{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS -fno-warn-orphans #-}
module Wishlist.Utils where

import Control.Monad.Except
import Prelude hiding (log)
import Servant.API

import Wishlist.Types

instance Show a => Show (Headers ls a) where
  show (Headers x _headers) = show x

log :: Show a => String -> Endpoint st a -> Endpoint st a
log msg action = do
  liftIO $ putStrLn $ "> " ++ msg ++ "..."
  val <- action `catchError` printError
  liftIO $ putStrLn $ "< " ++ show val
  return val
  where
    printError err = do
      liftIO $ putStrLn $ "ERROR: " ++ show err
      throwError err

logWith :: (Show a, Show b) => String -> a -> Endpoint st b -> Endpoint st b
logWith msg val action = log (msg ++ " " ++ show val) action
