{-| This module provides a brief introductory tutorial in the \"Introduction\"
    section followed by a lengthy discussion of the library's design and idioms.
-}

module Network.REST.Tutorial
       (
         -- * Introduction
         -- $intro

         -- * RESTful environments
         -- $environment

         -- * The RESTful monads
         -- $restful
       ) where

{- $intro

   The @REST@ client library is intended to make it simple to wrap REST API
   calls as Haskell functions.  For example, getting a Git tree from the
   GitHub API is as simple as:

> restful () [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/trees/#{sha}|]

   This is the stream-lined interface.  You can also express path components
   more programmatically, if desired.  More on this below.
-}

{- $environment

   Since calls to 'restful' involve the use of resources which must be cleaned
   up, such calls must be made in the context of a /RESTful environment/.  For
   example, here is a more complete example of the call above:

> withRestfulEnv (return ()) $ do
>    tree <- restful ...
>    -- perhaps inspect the tree here
>    return tree

   In this case, resources are fully resolved when 'withRestfulEnv' is done.
   The first parameter is a @RESTful ()@ monadic value (see below), which gets
   combined with each call to 'restful' or 'restfulEx' occurring in that
   environment.  This allows you to add custom request headers to every
   RESTful call, for example.

   If you want to hold the resource open longer ─ for example, to make
   multiple RESTful calls using the same HTTP 'Manager' object ─ you can pass
   the 'Manager' explicitly:

> withRestfulEnvAndMgr mgr (return ()) $ do
>    tree <- restful () [st|GET https://...|]
>    return tree

   Here the return value is in a 'ResourceT' transformer and can be bound with
   other calls to 'withRestfulEnvAndMgr'.
-}

{- $restful

   The @REST@ library uses two different monads: an environmental monad,
   described in the previous section, and another monad which is used to
   enrich 'restful' calls with other details.  This monad is used in two
   places: as the first argument to 'withRestfulEnv', and the last argument to
   'restfulEx'.  For example:

> withRestfulEnv (do addHeader "Range" "bytes 0-1000"
>                    addQueryParam "Foo" "Bar") $ do
>    restful () [st|GET ...|]
>    restfulEx () [st|GET ...|] $ do
>       addHeader "Authentication" "xxx"
>       addQueryParam "Key" "Value"

   In this code, both calls have the @Range@ header set and @Foo@ query
   parameter.  Only the second call includes the @Authentication@ header and
   the @Key@ parameter.
-}