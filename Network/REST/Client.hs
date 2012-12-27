{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Network.REST.Client where

import           Blaze.ByteString.Builder ( Builder, toByteString )
import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson hiding ((.=))
import           Data.Attempt
import           Data.ByteString as B ( ByteString, empty )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive
import           Data.Conduit
import           Data.Default ( Default(..) )
import           Data.Functor.Identity
import           Data.Monoid
import           Data.Text as T ( Text, empty, unpack, pack )
import qualified Data.Text.Encoding as E
import           Network.HTTP.Conduit as C
import           Network.HTTP.Types
import           Network.URI

type ContentType = ByteString
type RequestPath = Either Text (Query,[Text])
type RequestId   = Request Identity

data RESTfulEnv = RESTfulEnv
    { request      :: RequestId
    , remoteUri    :: Text
    , pathSegments :: [Text]
    , queryParams  :: QueryText }

_request :: Functor f =>
            (RequestId -> f RequestId) -> RESTfulEnv -> f RESTfulEnv
_request f env = f (request env) <&> \v -> env { request = v }
_uri :: Functor f => (Text -> f Text) -> RESTfulEnv -> f RESTfulEnv
_uri f env = f (remoteUri env) <&> \v -> env { remoteUri = v }
_path :: Functor f => ([Text] -> f [Text]) -> RESTfulEnv -> f RESTfulEnv
_path f env = f (pathSegments env) <&> \v -> env { pathSegments = v }
_query :: Functor f => (QueryText -> f QueryText) -> RESTfulEnv -> f RESTfulEnv
_query f env = f (queryParams env) <&> \v -> env { queryParams = v }

_method :: Functor f => (Method -> f Method) -> RequestId -> f RequestId
_method f req = f (method req) <&> \v -> req { method = v }
_host :: Functor f => (ByteString -> f ByteString) -> RequestId -> f RequestId
_host f req = f (host req) <&> \v -> req { host = v }
_port :: Functor f => (Int -> f Int) -> RequestId -> f RequestId
_port f req = f (port req) <&> \v -> req { port = v }
_secure :: Functor f => (Bool -> f Bool) -> RequestId -> f RequestId
_secure f req = f (secure req) <&> \v -> req { secure = v }
_headers :: Functor f =>
            (RequestHeaders -> f RequestHeaders) -> RequestId -> f RequestId
_headers f req = f (requestHeaders req) <&> \v -> req { requestHeaders = v }

instance Default RESTfulEnv where
  def = RESTfulEnv { request      = def
                   , remoteUri    = T.empty
                   , pathSegments = []
                   , queryParams  = []}

type RESTful a = State RESTfulEnv a

addPath :: Text -> RESTful ()
addPath segment = _path <>= [segment]

addDynPath :: (Show a) => a -> RESTful ()
addDynPath = addPath . pack . show

setUrl :: Text -> RESTful ()
setUrl = (_uri .=)

setMethod :: Method -> RESTful ()
setMethod = (_request._method .=)

addQueryKeyword :: Text -> RESTful ()
addQueryKeyword key = _query <>= [(key,Nothing)]

addQueryParam :: Text -> Text -> RESTful ()
addQueryParam key val = _query <>= [(key,Just val)]

addHeader :: Text -> Text -> RESTful ()
addHeader name val =
  _request._headers <>= [(mk (E.encodeUtf8 name), E.encodeUtf8 val)]

-- jww (2012-12-27): Test all of these encodings and decodings using fake
-- Arabic URIs.
utf8ToString :: Builder -> String
utf8ToString = T.unpack . E.decodeUtf8 . toByteString

getRequestUri :: Failure HttpException m => RESTfulEnv -> m URI
getRequestUri rest@(remoteUri -> "") =
  let req = request rest
  in return URI {
      uriScheme = if secure req then "https:" else "http:"
    , uriAuthority =
         Just URIAuth { uriUserInfo = ""
                      , uriRegName = BC.unpack $ host req
                      , uriPort = ':' : show (port req) }
    , uriPath     = utf8ToString (encodePathSegments (pathSegments rest))
    , uriQuery    = utf8ToString (renderQueryText True (queryParams rest))
    , uriFragment = "" }

getRequestUri rest =
  let ps  = pathSegments rest
      qs  = queryParams rest
      uri = parseURI (encodeUri (remoteUri rest))
  in case uri of
    Nothing -> failure $ InvalidUrlException (T.unpack (remoteUri rest))
                                            "Invalid Nothing"
    Just uri' -> return uri' {
        uriPath =
           if null ps
           then uriPath uri'
           else utf8ToString
                (encodePathSegments
                 (ps <> decodePathSegments (BC.pack (uriPath uri'))))
      , uriQuery =
           if null qs
           then uriQuery uri'
           else utf8ToString
                (renderQueryText True
                 (qs <> parseQueryText (BC.pack (uriQuery uri')))) }
  where encodeUri = escapeURIString isAllowedInURI . unpack

buildRequest :: Failure HttpException m => RESTfulEnv -> m (Request m)
buildRequest rest = do
  let reqi = request rest
  uri <- getRequestUri rest
  req <- parseUrl ((uriToString id uri) "")
  return req {
      method             = method reqi
    , clientCertificates = clientCertificates reqi
    , proxy              = proxy reqi
    , socksProxy         = socksProxy reqi
    , redirectCount      = redirectCount reqi
    , checkStatus        = checkStatus reqi
    , responseTimeout    = responseTimeout reqi
    , requestHeaders     = requestHeaders reqi
    , rawBody            = rawBody reqi
    , decompress         = decompress reqi }

type MonadRestfulInner m =
  -- jww (2012-12-26): MonadResource will change/move once I move withManager
  -- out the user level
  (MonadResource m, MonadBaseControl IO m, Failure HttpException m)

type MonadRestfulOuter m =
  (MonadIO m, MonadUnsafeIO m, MonadThrow m,
   MonadBaseControl IO m, Failure HttpException m)

restfulBody :: MonadRestfulInner m =>
               RequestBody (ResourceT m) -> RESTful ()
               -> m (ResumableSource (ResourceT m) ByteString)
-- jww (2012-12-26): withManager needs to move to the user scope, but we can
-- create a reader environment that may or may not receive a manager from the
-- user, and that environment will bring the manager down to here.
restfulBody body rest = withManager $ \mgr -> do
  req <- buildRequest $ execState rest (def :: RESTfulEnv)
  responseBody <$> http req { requestBody = body } mgr

restfulRawWith :: MonadRestfulInner m =>
                  RequestBody (ResourceT m) -> RESTful ()
                  -> m (Maybe ByteString)
restfulRawWith reqBody rest = do
  respBody <- restfulBody reqBody rest
  runResourceT $ respBody $$+- await

restfulRaw :: MonadRestfulInner m => RESTful () -> m (Maybe ByteString)
restfulRaw = restfulRawWith (RequestBodyBS B.empty)

restfulWith :: (MonadRestfulInner m, FromJSON a) =>
           RequestBody (ResourceT m) -> RESTful () -> m (Maybe a)
restfulWith reqBody rest = do
  content <- restfulRawWith reqBody rest
  return $ maybe Nothing (decode . BL.fromChunks . (:[])) content

restful :: (MonadRestfulInner m, FromJSON a) => RESTful () -> m (Maybe a)
restful = restfulWith (RequestBodyBS B.empty)

restfulUrlRaw :: MonadRestfulOuter m =>
                 Method -> RequestBody (ResourceT (ResourceT m)) -> Text
                 -> RESTful () -> m (Maybe ByteString)
restfulUrlRaw meth body url env =
  runResourceT $ restfulRawWith body $
    _request._method .= meth >> setUrl url >> env

restfulGetRaw :: MonadRestfulOuter m => Text -> m (Maybe ByteString)
restfulGetRaw url =
  restfulUrlRaw methodGet (RequestBodyBS B.empty) url (return ())

restfulGetRawEx :: MonadRestfulOuter m =>
                   Text -> RESTful () -> m (Maybe ByteString)
restfulGetRawEx = restfulUrlRaw methodGet (RequestBodyBS B.empty)

restfulHeadRaw :: MonadRestfulOuter m => Text -> m (Maybe ByteString)
restfulHeadRaw url =
  restfulUrlRaw methodHead (RequestBodyBS B.empty) url (return ())

restfulHeadRawEx :: MonadRestfulOuter m =>
                    Text -> RESTful () -> m (Maybe ByteString)
restfulHeadRawEx = restfulUrlRaw methodHead (RequestBodyBS B.empty)

restfulPostRawBS :: MonadRestfulOuter m =>
                    ByteString -> Text -> m (Maybe ByteString)
restfulPostRawBS body url =
  restfulUrlRaw methodPost (RequestBodyBS body) url (return ())

restfulPostRawBSEx :: MonadRestfulOuter m =>
                      ByteString -> Text -> RESTful () -> m (Maybe ByteString)
restfulPostRawBSEx body = restfulUrlRaw methodPost (RequestBodyBS body)

restfulPostRaw :: MonadRestfulOuter m =>
                  RequestBody (ResourceT (ResourceT m)) -> Text
                  -> m (Maybe ByteString)
restfulPostRaw body url = restfulUrlRaw methodPost body url (return ())

restfulPostRawEx :: MonadRestfulOuter m =>
                    RequestBody (ResourceT (ResourceT m)) -> Text
                    -> RESTful () -> m (Maybe ByteString)
restfulPostRawEx = restfulUrlRaw methodPost

restfulUrl :: (MonadRestfulOuter m, FromJSON a) =>
              Method -> RequestBody (ResourceT (ResourceT m)) -> Text
              -> RESTful () -> m (Maybe a)
restfulUrl meth body url env =
  runResourceT $ restfulWith body $
    _request._method .= meth >> setUrl url >> env

restfulGet :: (MonadRestfulOuter m, FromJSON a) => Text -> m (Maybe a)
restfulGet url =
  restfulUrl methodGet (RequestBodyBS B.empty) url (return ())

restfulGetEx :: (MonadRestfulOuter m, FromJSON a) => Text -> RESTful ()
                -> m (Maybe a)
restfulGetEx = restfulUrl methodGet (RequestBodyBS B.empty)

restfulHead :: (MonadRestfulOuter m, FromJSON a) => Text -> m (Maybe a)
restfulHead url =
  restfulUrl methodHead (RequestBodyBS B.empty) url (return ())

restfulHeadEx :: (MonadRestfulOuter m, FromJSON a) => Text -> RESTful ()
                 -> m (Maybe a)
restfulHeadEx = restfulUrl methodHead (RequestBodyBS B.empty)

restfulPostBS :: (MonadRestfulOuter m, FromJSON a) =>
                 ByteString -> Text -> m (Maybe a)
restfulPostBS body url =
  restfulUrl methodPost (RequestBodyBS body) url (return ())

restfulPostBSEx :: (MonadRestfulOuter m, FromJSON a) =>
                 ByteString -> Text -> RESTful () -> m (Maybe a)
restfulPostBSEx body = restfulUrl methodPost (RequestBodyBS body)

restfulPost :: (MonadRestfulOuter m, ToJSON a, FromJSON b) =>
               a -> Text -> m (Maybe b)
restfulPost v url =
  restfulUrl methodPost (RequestBodyLBS (encode (toJSON v))) url (return ())

restfulPostEx :: (MonadRestfulOuter m, ToJSON a, FromJSON b) =>
                 a -> Text -> RESTful () -> m (Maybe b)
restfulPostEx v = restfulUrl methodPost (RequestBodyLBS (encode (toJSON v)))

-- Client.hs