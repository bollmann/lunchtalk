Building micro-services in Haskell + Servant: Service APIs as Types
===================================================================

This repository contains the source files of two (very) simple
wishlist micro-services. Its purpose is to show how easy one can get
started with building micro-services in Haskell (and using the Servant
library).

The first service `wishlist-simple` is a wishlist service storing a
single user's personal wishes. The second service
`wishlist-multitenant` extends `wishlist-simple` to also store the
wishlists of multiple users.

How to run:
-----------

First, clone this git repository and `cd` into its root folder. Next,
to run the `wishlist-simple` service, execute the command `cabal run
wishlist-simple`. Similarly, to run the `wishlist-multitenant`
service, run `cabal run wishlist-multitenant`. The respective service
will then start locally on port `8080`, where you can query it
according to its API.

How to query:
-------------

* The `wishlist-simple` service provides the following (informal) API:
  ```
  GET  /wishes         -- retrieve all wishes stored so far on the service

  GET  /wishes/{shop}  -- retrieve all wishes at the given {shop}, which
	                   -- must be one of "amazon", "zalando", or "otto".

  POST /wishes         -- add a new wish to the wishlist, which needs to
                       -- be send in the request body as a JSON object of
                       -- of the form:
                       --
                       -- {"name": "my favorite wish!", "shop": "Amazon"}
                       --
                       -- where the wishes "shop" attribute must be either
                       -- one of "Amazon", "Zalando", or "Otto".
  ```

  For example, when the `wishlist-simple` service is running,
  we could retrieve the current wishlist of stored wishes using the
  following HTTP request:

  ```
  curl http://localhost:8080/wishes
  ```

  And to add a new wish, we would send the follwing HTTP request:

  ```
  curl \
    -H "Content-Type: application/json" \
	-d '{"name": "Game of Thrones 8", "shop": "Amazon"}' \
	http://localhost:8080/wishes
  ```

* The `wishlist-multitenant` service provides the same API as
  `wishlist-simple`, except that a client also always needs to provide
  a `Tenant` Header in its request indicating for which user he would
  like to retrieve a wishlist.

  So for example, to add a new wish to the wishlist of user `hans`,
  we would send the following HTTP request:

  ```
    curl \
    -H "Content-Type: application/json" \
	-H "Tenant: hans" \
	-d '{"name": "Game of Thrones 8", "shop": "Amazon"}' \
	http://localhost:8080/wishes
  ```

  And similarly, to retrieve `hans`s wishlist:

  ```
  curl -H "Tenant: hans" http://localhost:8080/wishes
  ```
