{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Network.REST.Client where

import           Blaze.ByteString.Builder (Builder, toByteString)
import           Control.Applicative
import           Control.Category
import           Control.Failure
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson hiding ((.=))
import           Data.ByteString as B hiding (pack)
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import           Data.Text
import qualified Data.Text.Lazy as TL
import           Network
import           Network.HTTP.Conduit
import           Network.HTTP.Types.Method
import           Network.HTTP.Types.URI
import           Prelude hiding ((.), id)
import           Text.Shakespeare.Text

data RESTful m = RESTful { restManager :: Manager
                         , restPath    :: [Text]
                         , restQuery   :: Query
                         , restRequest :: Request m }

restfulEnv :: Manager -> RESTful m
restfulEnv manager = RESTful { restManager = manager
                             , restPath    = []
                             , restQuery   = []
                             , restRequest = def }

type RESTfulT m a = StateT (RESTful m) m a

staticPath :: Monad m => Text -> RESTfulT m ()
staticPath segment =
  modify (\rest -> rest { restPath = restPath rest ++ [segment] })

dynamicPath :: (Monad m, Show a) => a -> RESTfulT m ()
dynamicPath = staticPath . pack . show

setApiURL :: (Failure HttpException m, Monad m)
             => TL.Text -> RESTfulT m ()
setApiURL url = do req <- parseUrl (TL.unpack url)
                   _request .= req
                   return ()

getRequest :: Monad m => RESTfulT m (Request m)
getRequest = liftM restRequest get

_request f rest = f (restRequest rest) <&> \v -> rest { restRequest = v }

_method f req = f (method req) <&> \v -> req { method = v }
_host f req = f (host req) <&> \v -> req { host = v }
_port f req = f (port req) <&> \v -> req { port = v }
_secure f req = f (secure req) <&> \v -> req { secure = v }

callAPI :: (MonadResource m, MonadBaseControl IO m)
           => RESTfulT m (Response (ResumableSource m ByteString))
callAPI = do
  rest <- get
  let req = restRequest rest
      mng = restManager rest
  lift $ http req { queryString = toByteString $
                                  encodePath (restPath rest)
                                             (restQuery rest) }
              mng

data Blob = Blob { blobContent  :: ByteString
                 , blobEncoding :: Text
                 , blobSha      :: Text
                 , blobSize     :: Int }
          deriving (Show, Eq)

instance FromJSON Blob where
  parseJSON (Object v) = Blob <$>
                         v .: "content" <*>
                         v .: "encoding" <*>
                         v .: "sha" <*>
                         v .: "size"

restAPI :: (MonadResource m, MonadBaseControl IO m,
            Failure HttpException m, FromJSON a)
           => Method -> TL.Text -> RESTfulT m (Maybe a)
restAPI meth url = do
  setApiURL url
  _request._method .= meth

  body    <- responseBody <$> callAPI
  content <- lift $ body $$+- await
  case content of
    Nothing -> return Nothing
    Just bs -> return . decode . BL.fromChunks . (:[]) $ bs

type RESTfulIO a = RESTfulT (ResourceT (ResourceT IO)) a

doGetRestful :: (FromJSON a) => RESTfulIO (Maybe a) -> IO (Maybe a)
doGetRestful f =
  withSocketsDo $ withManager $ \mgr ->
    runResourceT (evalStateT f (restfulEnv mgr))

{------------------------------------------------------------------------}

gitHubBlob :: Text -> Text -> Text -> RESTfulIO (Maybe Blob)
gitHubBlob owner repo sha =
  restAPI methodGet
    [lt|https://api.github.com/repos/#{owner}/#{repo}/git/blobs/#{sha}|]

main :: IO ()
main = print =<<
       (doGetRestful $
          gitHubBlob "fpco" "gitlib"
                     "d0047eb9166206e67bb12dbb6f1de65b89d6b68e")

-- Client.hs