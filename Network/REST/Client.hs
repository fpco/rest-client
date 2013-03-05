{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Network.REST.Client where

import           Blaze.ByteString.Builder ( Builder, toByteString )
import           Control.Applicative
import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson hiding ((.=), Success)
import           Data.Attempt
import           Data.ByteString as B ( ByteString, empty )
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.CaseInsensitive
import           Data.Conduit
import           Data.Conduit.List (consume)
import           Data.Default ( Default(..) )
import           Data.Functor.Identity
import           Data.Map hiding ( null )
import           Data.Monoid
import           Data.Proxy hiding (proxy)
import           Data.Text ( Text, unpack, pack )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Data.Typeable
import           Debug.Trace
import           Network.HTTP.Conduit as C hiding (Proxy)
import           Network.HTTP.Types
import           Network.URI
import           Prelude hiding (lookup)
import           Web.PathPieces

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
               else C.path y
    , queryString = if queryString y == BC.empty
                    then queryString x
                    else queryString y
    , requestBody =
        let empty = case requestBody y of
                RequestBodyLBS bs -> bs == BL.empty
                RequestBodyBS bs -> bs == B.empty
                _ -> False
        in if empty
           then requestBody x
           else requestBody y
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

addDynPath :: PathPiece a => a -> RESTful ()
addDynPath = addPath . toPathPiece

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

-- jww (2013-01-06): Instead of using shakespeare-text, look at the code and
-- build my own quasi-quoter specifically for the REST library
data RESTfulEnv = RESTfulEnv { restfulManager :: Manager
                           -- jww (2013-01-06): Check out Map TypeRep Dynamic
                           -- at https://github.com/yesodweb/yesod/issues/268
                           -- and allow arg lookup using the custom quasi-quoter
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
      -- jww (2013-01-25): This is just the wrong way to be doing this
      uri = parseURI (encodeUri (last . T.words . remoteUri $ rest))
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
  uri  <- getRequestUri rest
  req  <- parseUrl (uriToString id uri "")
  return req {
      -- jww (2013-01-25): This is far too fragile!
      method             = E.encodeUtf8 . head . T.words . remoteUri $ rest
    , secure             = uriScheme uri == "https:"
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
                      BL.ByteString -> RESTful ()
                      -> RESTfulEnvT m (ResumableSource m ByteString)
restfulMakeRequest body rest = do
  env <- ask
  req <- buildRequest $ execState (restfulPrereq env >> rest) def
  -- liftIO $ putStrLn $ "body: [" ++ show body ++ "]"
  -- liftIO $ putStrLn $ "restfulMakeRequest: " ++ show (req { requestBody = RequestBodyLBS body })
  responseBody <$> lift (http req { requestBody = RequestBodyLBS body }
                         (restfulManager env))

data TranslationException = TranslationException String
                          deriving (Show, Typeable)

instance Exception TranslationException

class TranslatableTo a v where
    wrap  :: a -> Proxy v -> BL.ByteString

class TranslatableFrom a v where
    unwrap :: BL.ByteString -> Proxy v -> Attempt a

class (TranslatableTo a v, TranslatableFrom a v) => Translatable a v

instance ToJSON a => TranslatableTo a Value where
    wrap x _ = encode x

instance FromJSON a => TranslatableFrom a Value where
    unwrap str _ =
        maybe (Failure (TranslationException
                        $ "Cannot unwrap ByteString of "
                        ++ show (BL.length str) ++ " bytes to JSON"))
              Success (decode str)

applyAndDecode ::
    (RestfulInner m, TranslatableFrom b v) =>
    Proxy v -> RESTfulEnvT m (ResumableSource m ByteString)
    -> RESTfulEnvT m (Attempt b)
applyAndDecode x action = do
  content <- action
  bs <- lift $ content $$+- consume
  return . flip unwrap x . BL.fromChunks $ bs

-- restfulGetAndDecode :: (RestfulInner m, TranslatableFrom b v) =>
--                        Proxy v -> RESTful () -> RESTfulEnvT m (Attempt b)
-- restfulGetAndDecode x = applyAndDecode x . restfulMakeRequest BL.empty

restfulRawEx_ :: RestfulInner m =>
                 BL.ByteString -> Text -> RESTful ()
                 -> RESTfulEnvT m (ResumableSource m ByteString)
restfulRawEx_ body url env =
  restfulMakeRequest body $ do
    let (meth,url') = T.span (==' ') url
    _request._method .= E.encodeUtf8 meth
    setUrl url'
    env

restfulRawEx :: RestfulInner m =>
                ByteString -> Text -> RESTful ()
                -> RESTfulEnvT m (ResumableSource m ByteString)
restfulRawEx body = restfulRawEx_ (BL.fromChunks [body])

restfulRaw :: RestfulInner m =>
              ByteString -> Text -> RESTfulEnvT m (ResumableSource m ByteString)
restfulRaw body url = restfulRawEx body url (return ())

restfulRawExL :: RestfulInner m =>
                 BL.ByteString -> Text -> RESTful ()
                 -> RESTfulEnvT m (ResumableSource m ByteString)
restfulRawExL body = restfulRawEx_ body

restfulRawL :: RestfulInner m =>
               BL.ByteString -> Text
               -> RESTfulEnvT m (ResumableSource m ByteString)
restfulRawL body url = restfulRawExL body url (return ())

restfulEx :: (RestfulInner m, TranslatableTo a v, TranslatableFrom b v) =>
             Proxy v -> a -> Text -> RESTful () -> RESTfulEnvT m (Attempt b)
restfulEx x val url env = applyAndDecode x $ restfulRawExL (wrap val x) url env

restful :: (RestfulInner m, TranslatableTo a v, TranslatableFrom b v) =>
           Proxy v -> a -> Text -> RESTfulEnvT m (Attempt b)
restful x val url = restfulEx x val url (return ())

restful_ :: (RestfulInner m, TranslatableTo a v) =>
            Proxy v -> a -> Text -> RESTfulEnvT m ()
restful_ x val url = void (restfulRawExL (wrap val x) url (return ()))

restfulJson :: (RestfulInner m,
                TranslatableTo a Value, TranslatableFrom b Value) =>
               a -> Text -> RESTfulEnvT m (Attempt b)
restfulJson = restful (Proxy :: Proxy Value)

restfulJson_ :: (RestfulInner m, TranslatableTo a Value) =>
               a -> Text -> RESTfulEnvT m ()
restfulJson_ = restful_ (Proxy :: Proxy Value)

restfulJsonEx :: (RestfulInner m,
                  TranslatableTo a Value, TranslatableFrom b Value) =>
                 a -> Text -> RESTful () -> RESTfulEnvT m (Attempt b)
restfulJsonEx = restfulEx (Proxy :: Proxy Value)

type RESTfulIO = RESTfulEnvT (ResourceT IO)

withRestfulEnvAndMgr ::
  MonadResource m => Manager -> RESTful () -> RESTfulEnvT m a -> m a
withRestfulEnvAndMgr mgr rest action =
  runReaderT action (RESTfulEnv { restfulManager = mgr
                                , restfulPrereq  = rest
                                , restfulArgs    = undefined })

class HasManager m where
    getManager   :: m Manager
    applyManager :: (MonadIO m, MonadBaseControl IO m, MonadThrow m,
                     MonadUnsafeIO m) => (Manager -> ResourceT m a) -> m a

instance HasManager IO where
    getManager   = newManager def
    applyManager = withManager

type RESTfulM a = forall (m :: * -> *).
                  (Failure HttpException m, MonadResource m,
                   MonadBaseControl IO m) => RESTfulEnvT m a

withRestfulEnv :: (MonadIO m, MonadBaseControl IO m, MonadThrow m,
                   MonadUnsafeIO m, HasManager m) =>
                  RESTful () -> RESTfulEnvT (ResourceT m) a -> m a
withRestfulEnv rest action =
  applyManager $ \mgr -> withRestfulEnvAndMgr mgr rest action

-- Client.hs