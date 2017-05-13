% Building micro-services in Haskell
% Service APIs as Types
% Dominik Bollmann -- May 12th, 2017

Let's build a Wishlist micro-service to store our wishes
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

How to build this Wishlist service in Haskell?
----------------------------------------------

### What's Haskell? ![The Haskell Logo](imgs/haskell-logo.png){width=150}\

* a strongly typed, purely functional programming language

### basic building blocks: Types and (pure) functions

* types: describe your data
* functions: transform your data

Haskell Types: how to model a `Wish` in Haskell
-----------------------------------------------

```haskell
data Wish = Wish { name :: String, shop :: Shop }

data Shop = Amazon | Zalando | Otto
```
. . .

```haskell
dvd :: Wish
dvd = Wish "Game of Thrones 8" Amazon

shoes :: Wish
shoes = Wish "Adidas Sneakers" Zalando
```

Haskell Types: how to model a `Wishlist` in Haskell
---------------------------------------------------

```haskell
data List a = Nil | Cons a (List a)

type Wishlist = List Wish
```
. . .

```haskell
emptyList :: List a
emptyList = Nil

myWishes :: List Wish
myWishes = Cons dvd (Cons shoes Nil)
  where
    dvd   = Wish "Game of Thrones 8" Amazon
	shoes = Wish "Adidas Sneakers" Zalando
```

Haskell functions: how to filter `Wishlist`s?
-------------------------------------------

```haskell
filter :: (a -> Bool) -> List a -> List a
filter _ Nil         = Nil
filter p (Cons x xs)
  | p x       = Cons x (filter p xs)
  | otherwise = filter p xs
```

. . .

```haskell


filter (\n -> n `mod` 2 == 0) (Cons 1 (Cons 2 (Cons 3 Nil)))
  == Cons 2 Nil
```

Haskell prelude end... how do we build the wishlist service?
---------------------------------------------

### Three easy steps thanks to Servant: ![Servant library](imgs/servant.png){width=70}\

1. formalize informal API as a type
2. define wishlist service wrt type spec.
3. run micro-service

Off to Emacs...
---------------

* Recap: informal API

```
GET  /wishes         -- get all my wishes
GET  /wishes/:shop   -- get all my wishes at :shop
                        (e.g., Amazon, Zalando)
POST /wishes         -- add a new wish to my wishlist
```

That's it! Let's see the service in action:
-------------------------------------------

URL: `https://haskell-wishlist.cfapps.us10.hana.ondemand.com`

. . .

Use `curl` queries to

* list all/shop wishes
* add a new wish

querying the wishlist service must conform to its API.

Benefits of an API as a Type
----------------------------

```haskell
type API =
       "wishes" :> Get '[JSON] Wishlist
  :<|> "wishes" :> Capture "shop" Shop
         :> Get '[JSON] Wishlist
  :<|> "wishes" :> ReqBody '[JSON] Wish
         :> Post '[JSON] ()
```

* this really *is* APIs first!

### Benefits

* The wishlist API is *explicit* in the program (compare!)
* The API type denotes a *live specification* (compare!)

the `API` type specifies the service/controllers:
-----------------------------------------------

```haskell

type API =
       "wishes" :> Get '[JSON] Wishlist
  :<|> "wishes" :> Capture "shop" Shop
         :> Get '[JSON] Wishlist
  :<|> "wishes" :> ReqBody '[JSON] Wish
         :> Post '[JSON] ()


service :: Service API
service = getAllWishes :<|> getShopWishes :<|> postNewWish

getAllWishes  :: Controller Wishlist
getShopWishes :: Shop -> Controller Wishlist
postNewWish   :: Wish -> Controller ()
```

This was simple... Can we create more expressive APIs?
------------------------------------------------------

### Yes! Servant APIs can also:

* require specific request headers in requests

. . .

* enforce specific response headers in controller responses

. . .

* ensure even type-safe links: all used links are within `API`

Conclusion
----------

Haskell + Servant: interesting approach using "APIs as Types".

Benefits:

1. explicit, formal, live spec/type for a service.
2. yields services that are faithful wrt their API.
3. clients and docs come (almost) for free

* Servant offers much more!

References
----------

* Code: `https://github.com/bollmann/lunchtalk.git`

1. Benjamin C. Pierce. The Science of Deep Specification, Nov 2015. https://www.youtube.com/watch?v=Y2jQe8DFzUM
2. Julian Arni. Servant: a type-level DSL for web APIs, July, 2015. https://www.youtube.com/watch?v=snOBI8PcbMQ
3. Servant Contributors. The Servant Library. https://hackage.haskell.org/package/servant
4. Paul Hudak, et al. A History of Haskell: Being Lazy with Class. 2007. http://haskell.cs.yale.edu/wp-content/uploads/2011/02/history.pdf

Compare to implicit APIs:
-------------------------

```java
@Path("/wishes")
public class Wishes {

  @GET
  @Produces("application/json")
  GetWishesResponse getWishes() { ... }

  @POST
  @Consumes("application/json")
  PostWishesResponse postWishes(Wish entity) { ... }

  @GET
  @Path("/{shop}")
  @Produces("application/json")
  GetWishesByShopResponse getWishesByShop(
    @PathParam("shop") String shop) { ... }
}
```

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
* Enforce `Wish-Count` response header to be sent in `GET` responses

Adjust (enrich) our wishlist's formal API:
------------------------------------------

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

New API type guides service refactoring:
----------------------------------------

* no changes required here

```haskell
service :: Service API
service = getAllWishes :<|> getShopWishes :<|> postNewWish
```

New API type requires changes to `getAllWishes`
-----------------------------------------------

```haskell
type RichWishlist =
  Headers '[Header "Wish-Count" Int] Wishlist

getAllWishes :: Maybe Tenant -> Controller RichWishlist
getAllWishes Nothing = do
  let noTenant = "you must provide a Tenant header!"
  throwError err400 { errBody = noTenant }
```
. . .
```haskell
getAllWishes (Just tenant) = do
  wishlist <- ... -- find tenant's wishlist in store
  pure $ addHeader (length wishlist) wishlist
```

Static (compile-time) guarantees wrt the API spec.
----------------------------------------------

1. controller must take into account a `Maybe Tenant`
2. controller must return a `RichWishlist` with the `Wish-Count` header

going further:

* ensure even type-safe links: all used links are within `API`.
