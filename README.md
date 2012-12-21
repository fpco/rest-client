rest-client
-----------

rest-client is a Haskell library for quickly and easily creating RESTful API
clients.

The main idea is to use a set of combinators for construct the components of a
query, which consist of:

    Type (GET, HEAD, POST)
    Path
    Query string
    Request body (more structured data, a data blob, or both)
    Parsing or returning the response body

One idea we have so far is to construct a DSL within a REST Monad.  I'll use
the example of the 'createBlob' API function from
[GitHub](http://developer.github.com/v3/git/blobs/).  Here is the
specification for that API call:

    POST /repos/:owner/:repo/git/blobs

    Input
    {
      "content": "Content of the blob",
      "encoding": "utf-8"
    }

    Response

    Status: 201 Created
    Location: https://api.github.com/git/:owner/:repo/blob/:sha
    X-RateLimit-Limit: 5000
    X-RateLimit-Remaining: 4999

    {
      "sha": "3a0f86fb8db8eea7ccbb9a95f325ddbedfb25e15"
    }

The monadic DSL for such a query might look like this:

    class RESTResponse a where
        restResponse :: ResponseBody (ResumableSource ByteString) -> m a

    newtype AsJSON a = AsJSON a

    instance FromJSON a => RESTResponse (AsJSON a)

    createBlob :: Text -> Text -> ByteString
                    -> RESTfulT IO (Text,Text,MyDataType,ByteString)
    createBlob owner repo content = do
      pathSegmentStatic "repos"
      pathSegmentDynamic owner
      pathSegmentDynamic repo
      pathSegmentStatic "git/blobs"

      -- queryParam "key" value
      
      requestParam "content" (E.encodeUtf8 content)
      requestParam "encoding" "utf-8"
      
      (,,) <$> responseParam "sha"
           <*> responseMetadata "Location" 
           <*> decode responseBody
           <*> responseBody

There are four different types of repsonse decoding here:

 - `responseParam` determines from type inference that "sha" should be decoded
   from JSON (the default) as a Text value
   
 - `responseMetadata` is always a Text value pulled out of the repsonse
    headers.  TODO: There should be a provision to auto-convert these to
    common types, like a Date
 
 - `responseBody` by default yields a strict ByteString.  But depending on the
    type there are other options:
    
    - `ByteString`
    - `ResumableSource (ResourceT IO) ByteString`
    - some data type `a`, in which case the data is decoded from JSON by
      calling 'decode responseBody'.  (Is this sufficient?)

Authentication details are maintained as State within RESTfulT, so the call to
initiate such a transaction might look like this:

    sha <- runREST auth $ createBlob "jwiegley" "rest-client" someByteString

This `auth` parameter could provide any context the API library designer
wishes, and there would be callbacks within the library to get back to that
information.  For example, if the authentication scheme requires passes
certain request parameters along with every request, this would avoid
duplicating that fact in every method of the client library.

More ideas:

 - Can we get away with just using an applicative interface, or is a Monad
   really the best option?
   
 - Need a way to specify custom behavior, like a PUT request that is chunked
   over several PUTs (AWS has an API call that requires this for large request
   bodies).
