{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Network.REST.Client where

import           Blaze.ByteString.Builder ( Builder, toByteString )
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson hiding ( (.=) )
import           Data.Attempt
import           Data.ByteString as B ( ByteString )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive
import           Data.Conduit
import           Data.Default ( Default(..) )
import           Data.Functor.Identity
import           Data.Map hiding ( null )
import           Data.Monoid
import           Data.Text ( Text, unpack, pack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Network.HTTP.Conduit as C
import           Network.HTTP.Types
import           Network.URI
import           Prelude hiding (lookup)

type ContentType = ByteString
type RequestId   = Request Identity

_method f req  = f (method req)         <&> \v -> req { method = v }
_host f req    = f (host req)           <&> \v -> req { host = v }
_port f req    = f (port req)           <&> \v -> req { port = v }
_secure f req  = f (secure req)         <&> \v -> req { secure = v }
_headers f req = f (requestHeaders req) <&> \v -> req { requestHeaders = v }
_body f req    = f (requestBody req)    <&> \v -> req { requestBody = v }

mergeRequests :: RequestId -> RequestId -> RequestId
mergeRequests x y =
  x { host = if host y == "localhost"
             then host x
             else host y
    , port = if port y == 80
             then port x
             else port y
    , secure = secure x || secure y
    , clientCertificates = clientCertificates x <> clientCertificates y
    , requestHeaders = requestHeaders x <> requestHeaders y
    , C.path = if C.path y == "/"
               then C.path x
               else C.path x
    , queryString = if queryString y == BC.empty
                    then queryString x
                    else queryString y
    , requestBody = requestBody y
    , method = if method y == "GET"
               then method x
               else method y
    , proxy = maybe (proxy x) Just (proxy y)
    , socksProxy = maybe (socksProxy x) Just (socksProxy y)
    , rawBody = rawBody x || rawBody y
    , decompress = decompress y
    , redirectCount = if redirectCount y == 10
                      then redirectCount x
                      else redirectCount y
    , checkStatus = checkStatus y
    , responseTimeout = maybe (responseTimeout x) Just (responseTimeout y) }

data RESTfulRequest = RESTfulRequest { request      :: RequestId
                                     , remoteUri    :: Text
                                     , pathSegments :: [Text]
                                     , requestVars  :: Map Text Text
                                     , queryParams  :: QueryText }

_request f env = f (request env)      <&> \v -> env { request = v }
_uri f env     = f (remoteUri env)    <&> \v -> env { remoteUri = v }
_path f env    = f (pathSegments env) <&> \v -> env { pathSegments = v }
_query f env   = f (queryParams env)  <&> \v -> env { queryParams = v }
_vars f env    = f (requestVars env)  <&> \v -> env { requestVars = v }

type RESTful = State RESTfulRequest

instance Default RESTfulRequest where
  def = RESTfulRequest { request      = def
                       , remoteUri    = T.empty
                       , pathSegments = []
                       , requestVars  = Data.Map.empty
                       , queryParams  = [] }

instance Monoid RESTfulRequest where
  mempty = def
  x `mappend` y =
    RESTfulRequest {
        request      = mergeRequests (request x) (request y)
      , remoteUri    = if T.null (remoteUri y)
                       then remoteUri x
                       else remoteUri y
      , pathSegments = pathSegments x `mappend` pathSegments y
      , requestVars  = fromList (toList (requestVars x) `mappend`
                                 toList (requestVars y))
      , queryParams  = queryParams x  `mappend` queryParams y }

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

getVar :: Monad m => Text -> RESTfulEnvT m (Maybe Text)
getVar key = do env <- ask
                return $ lookup key (restfulArgs env)

addHeader :: Text -> Text -> RESTful ()
addHeader name val =
  _request._headers <>= [(mk (E.encodeUtf8 name), E.encodeUtf8 val)]

data RESTfulEnv = RESTfulEnv { restfulManager :: Manager
                             , restfulArgs    :: Map Text Text
                             , restfulPrereq  :: RESTful () }

type RESTfulEnvT = ReaderT RESTfulEnv

-- jww (2012-12-27): Review these encodings and decodings to ensure nothing
-- get lost or added that shouldn't be along the way.
utf8ToString :: Builder -> String
utf8ToString = T.unpack . E.decodeUtf8 . toByteString

getRequestUri :: Failure HttpException m => RESTfulRequest -> RESTfulEnvT m URI
getRequestUri rest@(remoteUri -> "") =
  let req = request rest
  in return URI {
      uriScheme    = if secure req then "https:" else "http:"
    , uriAuthority = Just URIAuth { uriUserInfo = ""
                                  , uriRegName = BC.unpack $ host req
                                  , uriPort = ':' : show (port req) }
    , uriPath      = utf8ToString (encodePathSegments (pathSegments rest))
    , uriQuery     = utf8ToString (renderQueryText True (queryParams rest))
    , uriFragment  = "" }

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

buildRequest :: Failure HttpException m =>
                RESTfulRequest -> RESTfulEnvT m (Request m)
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

type RestfulInner m =
  (Failure HttpException m, MonadResource m, MonadBaseControl IO m)

restfulMakeRequest :: RestfulInner m =>
                      RESTful () -> RESTfulEnvT m (ResumableSource m ByteString)
restfulMakeRequest rest = do
  env <- ask
  let env' = execState (restfulPrereq env >> rest) (def :: RESTfulRequest)
  req <- buildRequest env'
  responseBody <$> lift (http req (restfulManager env))

restfulGetResponseBody :: RestfulInner m =>
                          RESTful () -> RESTfulEnvT m (Maybe ByteString)
restfulGetResponseBody rest = do
  respBody <- restfulMakeRequest rest
  lift $ respBody $$+- await

restfulGetAndDecode :: (RestfulInner m, FromJSON b) =>
                       RESTful () -> RESTfulEnvT m (Maybe b)
restfulGetAndDecode rest = do
  content <- restfulGetResponseBody rest
  return $ maybe Nothing (decode . BL.fromChunks . (:[])) content

restfulRawEx_ :: RestfulInner m =>
                 RequestBody Identity -> Text -> RESTful ()
                 -> RESTfulEnvT m (Maybe ByteString)
restfulRawEx_ body url env =
  restfulGetResponseBody $ do
    let (meth,url') = T.span (==' ') url
    _request._method .= E.encodeUtf8 meth
    _request._body .= body
    setUrl url'
    env

restfulRawEx :: RestfulInner m =>
                ByteString -> Text -> RESTful ()
                -> RESTfulEnvT m (Maybe ByteString)
restfulRawEx body = restfulRawEx_ (RequestBodyBS body)

restfulRaw :: RestfulInner m =>
              ByteString -> Text -> RESTfulEnvT m (Maybe ByteString)
restfulRaw body url = restfulRawEx body url (return ())

restfulRawExL :: RestfulInner m =>
                 BL.ByteString -> Text -> RESTful ()
                 -> RESTfulEnvT m (Maybe ByteString)
restfulRawExL body = restfulRawEx_ (RequestBodyLBS body)

restfulRawL :: RestfulInner m =>
               BL.ByteString -> Text -> RESTfulEnvT m (Maybe ByteString)
restfulRawL body url = restfulRawExL body url (return ())

restfulEx :: (RestfulInner m, ToJSON a, FromJSON b) =>
             a -> Text -> RESTful () -> RESTfulEnvT m (Maybe b)
restfulEx val url env = do
  response <- restfulRawExL (encode val) url env
  return $ maybe Nothing (decode . BL.fromChunks . (:[])) response

restful :: (RestfulInner m, ToJSON a, FromJSON b) =>
           a -> Text -> RESTfulEnvT m (Maybe b)
restful val url = restfulEx val url (return ())

restful_ :: (RestfulInner m, ToJSON a) => a -> Text -> RESTfulEnvT m ()
restful_ val url = void (restfulRawExL (encode val) url (return ()))

type RESTfulIO = RESTfulEnvT (ResourceT IO)

withRestfulEnvAndMgr ::
  Monad m =>
  Manager -> RESTful () -> RESTfulEnvT (ResourceT m) a -> ResourceT m a
withRestfulEnvAndMgr mgr rest action =
  runReaderT action
    (RESTfulEnv { restfulManager = mgr
                , restfulPrereq  = rest })

type RESTfulM a = forall (m :: * -> *).
                  (Failure HttpException m, MonadResource m,
                   MonadBaseControl IO m) => RESTfulEnvT m a

withRestfulEnv :: (MonadIO m, MonadUnsafeIO m, MonadBaseControl IO m,
                   MonadThrow m) =>
                  RESTful () -> RESTfulEnvT (ResourceT m) a -> m a
withRestfulEnv rest action =
  withManager $ \mgr -> withRestfulEnvAndMgr mgr rest action

-- Client.hs