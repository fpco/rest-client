{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module Network.REST.Client where

import           Blaze.ByteString.Builder ( toByteString )
import           Control.Applicative
import           Control.Exception ( SomeException )
import           Control.Lens ( (.=), (%=), (<&>) )
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Lazy ( State, execState )
import           Data.Aeson hiding ((.=))
import           Data.Attempt
import           Data.ByteString as B ( ByteString, empty )
import qualified Data.ByteString.Char8 as BC ( unpack )
import qualified Data.ByteString.Lazy as BL ( fromChunks )
import           Data.CaseInsensitive
import           Data.Certificate.X509 ( X509 )
import           Data.Conduit
import           Data.Default ( Default(..) )
import           Data.Maybe ( fromMaybe )
import           Data.Text as T ( Text, empty, unpack, pack )
import qualified Data.Text.Encoding as E ( encodeUtf8 )
import           Data.Tuple ( swap )
import qualified Network.HTTP.Conduit as C
import           Network.HTTP.Types
import           Network.Socks5 ( SocksConf )
import           Network.TLS ( PrivateKey )

type ContentType = ByteString
type RequestPath = Either Text (Query,[Text])

data PreRequest = PreRequest
    { method :: Method                  -- HTTP request method, eg GET, POST.
    , path :: RequestPath
    , host :: Text
    , port :: Int
    , secure :: Bool                    -- Whether to use HTTPS (ie, SSL).
    , clientCertificates :: [(X509, Maybe PrivateKey)]
                                       -- SSL client certificates

    , proxy :: Maybe C.Proxy            -- Optional HTTP proxy.
    , socksProxy :: Maybe SocksConf     -- Optional SOCKS proxy.

    , redirectCount :: Int              -- How many redirects to follow when
                                       -- getting a resource. 0 means follow
                                       -- no redirects. Default value: 10.
    , checkStatus :: Maybe (Status -> ResponseHeaders -> Maybe SomeException)
                                       -- Check the status code. Note that
                                       -- this will run after all redirects
                                       -- are performed. Default: return a
                                       -- @StatusCodeException@ on non-2XX
                                       -- responses.
    , responseTimeout :: Maybe Int      -- Number of microseconds to wait for a
                                       -- response. If @Nothing@, will wait
                                       -- indefinitely. Default: 5 seconds.

    , requestHeaders :: RequestHeaders  -- Custom HTTP request headers

    , rawBody :: Bool                   -- If @True@, a chunked and\/or gzipped
                                       -- body will not be decoded. Use with
                                       -- caution.
    , decompress :: ContentType -> Bool  -- Predicate to specify whether gzipped
                                       -- data should be decompressed on the
                                       -- fly (see 'alwaysDecompress' and
                                       -- 'browserDecompress'). Default:
                                       -- browserDecompress.
    }

_method :: Functor f => (Method -> f Method) -> PreRequest -> f PreRequest
_method f req = f (method req) <&> \v -> req { method = v }
_path :: Functor f =>
         (RequestPath -> f RequestPath) -> PreRequest -> f PreRequest
_path f req   = f (path req)   <&> \v -> req { path   = v }
_host :: Functor f => (Text -> f Text) -> PreRequest -> f PreRequest
_host f req   = f (host req)   <&> \v -> req { host   = v }
_port :: Functor f => (Int -> f Int) -> PreRequest -> f PreRequest
_port f req   = f (port req)   <&> \v -> req { port   = v }
_secure :: Functor f => (Bool -> f Bool) -> PreRequest -> f PreRequest
_secure f req = f (secure req) <&> \v -> req { secure = v }
_headers :: Functor f =>
            (RequestHeaders -> f RequestHeaders) -> PreRequest -> f PreRequest
_headers f req = f (requestHeaders req) <&> \v -> req { requestHeaders = v }

instance Default PreRequest where
  def = PreRequest { method             = methodGet
                   , path               = Right ([],[])
                   , host               = T.empty
                   , port               = 80
                   , secure             = False
                   , clientCertificates = []
                   , proxy              = Nothing
                   , socksProxy         = Nothing
                   , redirectCount      = 10
                   , checkStatus        = Nothing
                   , responseTimeout    = Nothing
                   , requestHeaders     = []
                   , rawBody            = False
                   , decompress         = C.browserDecompress }

type RESTful a = State PreRequest a

addPath :: Text -> RESTful ()
addPath segment = _path %= (((++ [segment]) <$>) <$>)

addDynPath :: (Show a) => a -> RESTful ()
addDynPath = addPath . pack . show

setUrl :: Text -> RESTful ()
setUrl = (_path .=) . Left

addHeader :: Text -> Text -> RESTful ()
addHeader name value =
  _headers %= (++ [(mk (E.encodeUtf8 name), E.encodeUtf8 value)])

buildRequest :: Failure C.HttpException m => PreRequest -> m (C.Request m)
buildRequest p = do
  req <- C.parseUrl (either unpack buildUrl (path p))
  let req' = req {
          C.method             = method p
        , C.clientCertificates = clientCertificates p
        , C.proxy              = proxy p
        , C.socksProxy         = socksProxy p
        , C.redirectCount      = redirectCount p
        , C.checkStatus        = fromMaybe (C.checkStatus req) (checkStatus p)
        , C.responseTimeout    = responseTimeout p
        , C.requestHeaders     = requestHeaders p
        , C.rawBody            = rawBody p
        , C.decompress         = decompress p }

  case path p of
    Left _ -> return req'
    Right _ -> return req' {
        C.host   = E.encodeUtf8 (host p)
      , C.port   = port p
      , C.secure = secure p }

  where buildUrl = BC.unpack . toByteString . uncurry encodePath . swap

type MonadRestfulInner m =
  (MonadResource m, MonadBaseControl IO m, Failure C.HttpException m)

type MonadRestfulOuter m =
  (MonadIO m, MonadUnsafeIO m, MonadThrow m,
   MonadBaseControl IO m, Failure C.HttpException m)

restfulBody :: MonadRestfulInner m =>
               C.RequestBody (ResourceT m) -> RESTful ()
               -> m (ResumableSource (ResourceT m) ByteString)
restfulBody body rest = C.withManager $ \mgr -> do
  req <- buildRequest $ flip execState (def :: PreRequest) rest
  C.responseBody <$> C.http req { C.requestBody = body } mgr

restfulRawWith :: MonadRestfulInner m =>
                  C.RequestBody (ResourceT m) -> RESTful ()
                  -> m (Maybe ByteString)
restfulRawWith reqBody rest = do
  respBody <- restfulBody reqBody rest
  runResourceT $ respBody $$+- await

restfulRaw :: MonadRestfulInner m => RESTful () -> m (Maybe ByteString)
restfulRaw = restfulRawWith (C.RequestBodyBS B.empty)

restfulWith :: (MonadRestfulInner m, FromJSON a) =>
           C.RequestBody (ResourceT m) -> RESTful () -> m (Maybe a)
restfulWith reqBody rest = do
  content <- restfulRawWith reqBody rest
  return $ maybe Nothing (decode . BL.fromChunks . (:[])) content

restful :: (MonadRestfulInner m, FromJSON a) => RESTful () -> m (Maybe a)
restful = restfulWith (C.RequestBodyBS B.empty)

restfulUrlRaw :: MonadRestfulOuter m =>
                 Method -> C.RequestBody (ResourceT (ResourceT m)) -> Text
                 -> RESTful () -> m (Maybe ByteString)
restfulUrlRaw meth body url env =
  runResourceT $ restfulRawWith body $ _method .= meth >> setUrl url >> env

restfulGetRaw :: MonadRestfulOuter m => Text -> m (Maybe ByteString)
restfulGetRaw url =
  restfulUrlRaw methodGet (C.RequestBodyBS B.empty) url (return ())

restfulGetRawEx :: MonadRestfulOuter m =>
                   Text -> RESTful () -> m (Maybe ByteString)
restfulGetRawEx = restfulUrlRaw methodGet (C.RequestBodyBS B.empty)

restfulHeadRaw :: MonadRestfulOuter m => Text -> m (Maybe ByteString)
restfulHeadRaw url =
  restfulUrlRaw methodHead (C.RequestBodyBS B.empty) url (return ())

restfulHeadRawEx :: MonadRestfulOuter m =>
                    Text -> RESTful () -> m (Maybe ByteString)
restfulHeadRawEx = restfulUrlRaw methodHead (C.RequestBodyBS B.empty)

restfulPostRawBS :: MonadRestfulOuter m =>
                    ByteString -> Text -> m (Maybe ByteString)
restfulPostRawBS body url =
  restfulUrlRaw methodPost (C.RequestBodyBS body) url (return ())

restfulPostRawBSEx :: MonadRestfulOuter m =>
                      ByteString -> Text -> RESTful () -> m (Maybe ByteString)
restfulPostRawBSEx body = restfulUrlRaw methodPost (C.RequestBodyBS body)

restfulPostRaw :: MonadRestfulOuter m =>
                  C.RequestBody (ResourceT (ResourceT m)) -> Text
                  -> m (Maybe ByteString)
restfulPostRaw body url = restfulUrlRaw methodPost body url (return ())

restfulPostRawEx :: MonadRestfulOuter m =>
                    C.RequestBody (ResourceT (ResourceT m)) -> Text
                    -> RESTful () -> m (Maybe ByteString)
restfulPostRawEx body = restfulUrlRaw methodPost body

restfulUrl :: (MonadRestfulOuter m, FromJSON a) =>
              Method -> C.RequestBody (ResourceT (ResourceT m)) -> Text
              -> RESTful () -> m (Maybe a)
restfulUrl meth body url env =
  runResourceT $ restfulWith body $ _method .= meth >> setUrl url >> env

restfulGet :: (MonadRestfulOuter m, FromJSON a) => Text -> m (Maybe a)
restfulGet url =
  restfulUrl methodGet (C.RequestBodyBS B.empty) url (return ())

restfulGetEx :: (MonadRestfulOuter m, FromJSON a) => Text -> RESTful ()
                -> m (Maybe a)
restfulGetEx = restfulUrl methodGet (C.RequestBodyBS B.empty)

restfulHead :: (MonadRestfulOuter m, FromJSON a) => Text -> m (Maybe a)
restfulHead url =
  restfulUrl methodHead (C.RequestBodyBS B.empty) url (return ())

restfulHeadEx :: (MonadRestfulOuter m, FromJSON a) => Text -> RESTful ()
                 -> m (Maybe a)
restfulHeadEx = restfulUrl methodHead (C.RequestBodyBS B.empty)

restfulPostBS :: (MonadRestfulOuter m, FromJSON a) =>
                 ByteString -> Text -> m (Maybe a)
restfulPostBS body url =
  restfulUrl methodPost (C.RequestBodyBS body) url (return ())

restfulPostBSEx :: (MonadRestfulOuter m, FromJSON a) =>
                 ByteString -> Text -> RESTful () -> m (Maybe a)
restfulPostBSEx body = restfulUrl methodPost (C.RequestBodyBS body)

restfulPost :: (MonadRestfulOuter m, ToJSON a, FromJSON b) =>
               a -> Text -> m (Maybe b)
restfulPost v url =
  restfulUrl methodPost (C.RequestBodyLBS (encode (toJSON v))) url (return ())

restfulPostEx :: (MonadRestfulOuter m, ToJSON a, FromJSON b) =>
                 a -> Text -> RESTful () -> m (Maybe b)
restfulPostEx v url env =
  restfulUrl methodPost (C.RequestBodyLBS (encode (toJSON v))) url env

-- Client.hs