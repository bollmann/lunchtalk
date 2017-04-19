% Building micro-services in Haskell
% The benefits of service APIs as Types
% Dominik Bollmann -- May, 9th, 2017

![The Haskell Logo](imgs/haskell.png){width=350}

* basic building blocks: Types and (pure) functions
<!-- * distinctive features: lazyness and typeclasses -->
* It's very exciting and made me rethink programming!


Haskell Types: describe your data
---------------------------------

```haskell
data Wish = Wish
  { name :: String
  , shop :: Shop
  }

data Shop = Amazon | Zalando | Otto
```
. . .

```haskell
dvd :: Wish
dvd = Wish "Game of Thrones 8" Amazon

shoes :: Wish
shoes = Wish "Adidas Sneakers" Zalando
```


Types and Functions
-------------------

```haskell
data List a = Nil | Cons a (List a)
type Wishlist = List Wish
```
. . .

### Haskell functions: transform your data

```haskell
filter :: (a -> Bool) -> List a -> List a
filter _ Nil         = Nil
filter p (Cons x xs)
  | p x       = Cons x (filter p xs)
  | otherwise = filter p xs
```

. . .

### A tower of abstractions on top!

* functors, applicatives, monads, ...
* advanced type-level machinery


Problem: Let's build a Wishlist micro-service in Haskell
--------------------------------------------------------

* informal API:

```
GET  /wishes         -- get all my wishes
GET  /wishes/:shop   -- get all my wishes at :shop
                        (e.g., Amazon, Zalando)
POST /wishes         -- add a new wish to my wishlist
```

. . .

* Wishes serialized as JSON:

```json
{ "name": "Game of Thrones 8", "shop": "Amazon" }
{ "name": "Adidas Sneakers", "shop": "Zalando" }
```


Step 1): Formalize the API as a Haskell Type
--------------------------------------------

```haskell
type API =
       "wishes" :> Get '[JSON] Wishlist
  :<|> "wishes" :> Capture "shop" Shop
         :> Get '[JSON] Wishlist
  :<|> "wishes" :> ReqBody '[JSON] Wish
         :> Post '[JSON] ()

```
. . .

* this really *is* APIs first!
* The wishlist API is explicit in the program (compare!)
* The API type denotes a live specification (compare!)


Step 2): the API defines the controller types:
----------------------------------------------

```haskell
type Store = IORef Wishlist

service :: Service Store API
service = getAllWishes
  :<|> getShopWishes
  :<|> postNewWish


getAllWishes :: Controller Store Wishlist
getAllWishes = do
  store <- ask
  liftIO (readIORef store)
```

---

```haskell
getShopWishes :: Shop -> Controller Store Wishlist
getShopWishes shop = do
  wishlist <- getAllWishes
  pure $ filter (\wish -> getShop wish == shop) wishlist


postNewWish :: Wish -> Controller Store ()
postNewWish wish = do
  store <- ask
  liftIO $ do
    wishlist <- readIORef store
    writeIORef store (wish:wishlist)
```

Step 3): Running the service
----------------------------

```haskell
main :: IO ()
main = do
  putStrLn "Starting wishlist service on port 8080..."
  store <- newIORef []
  run 8080 $ serve proxy service'
  where
    proxy    = Proxy :: Proxy API
    service' = enter (toHandler store) service
```

That's it! Let's see the service in action:
-------------------------------------------

Use `curl` queries to

* list all/shop wishes
* add a new wish

querying the wishlist service must conform to its API.

This was simple: could we build a multi-tenant service, too?
------------------------------------------------------------

```
GET  /wishes         -- get all my wishes
GET  /wishes/:shop   -- get all my wishes at :shop
                        (e.g., Amazon, Zalando)
POST /wishes         -- add a new wish to my wishlist
```

### New requirements:

* Require `Tenant` request header on all requests
* Enforce `Wish-Count` response header to be sent in all `GET` responses

Adjust (enrich) our wishlist's formal API
-----------------------------------------

```haskell
type API =
      "wishes" :> Header "Tenant" Tenant
	    :> Get '[JSON] RichWishlist
 :<|> "wishes" :> Header "Tenant" Tenant
        :> Capture "shop" Shop
		:> Get '[JSON] RichWishlist
 :<|> "wishes" :> Header "Tenant" Tenant
        :> ReqBody '[JSON] Wish :> Post '[JSON] ()

type RichWishlist =
  Headers '[Header "Wish-Count" Int] Wishlist
```

New API specification guides service refactoring
--------------------------------------------------

```haskell
type TenantStore = IORef (Map Tenant Wishlist)

server :: Service TenantStore API
server = getAllWishes
  :<|> getShopWishes
  :<|> postNewWish
```

---

```haskell
type RichWishlist =
  Headers '[Header "Wish-Count" Int] Wishlist

getAllWishes
  :: Maybe Tenant -> Controller TenantStore RichWishlist
getAllWishes Nothing = do
  let noTenant = "you must provide a Tenant header!"
  throwError err400 { errBody = noTenant }
getAllWishes (Just tenant) = do
  store   <- ask
  tenants <- liftIO (readIORef store)
  case Map.lookup tenant tenants of
    Just wishlist ->
	  pure $ addHeader (length wishlist) wishlist
    Nothing -> pure $ addHeader 0 []
```

Static (compile-time) guarantees wrt the API spec.
----------------------------------------------

1. controller must take into account a `Maybe Tenant`
2. controller must return a `RichWishlist` with the `Wish-Count` header

going further:

* ensure even type-safe links: all used links are within `API`.


Conclusion
----------

Haskell + Servant: interesting approach using "APIs as Types".

Benefits:

1. explicit, formal, live spec/type for a service.
2. yields services that are faithful wrt their API.
3. Haskell's type system catches many errors at compile-time.

References
----------

1. Benjamin C. Pierce. The Science of Deep Specification, Nov 2015. https://www.youtube.com/watch?v=Y2jQe8DFzUM
2. Julian Arni. Servant: a type-level DSL for web APIs, July, 2015. https://www.youtube.com/watch?v=snOBI8PcbMQ
3. Servant Contributors. The Servant Library. https://hackage.haskell.org/package/servant
