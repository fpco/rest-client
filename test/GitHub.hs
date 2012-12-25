{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.ByteString as B ( ByteString, empty )
import Data.Default ( Default(..) )
import Data.Monoid
import Data.Text hiding (drop)
import Data.Text.Encoding as E
import Network.REST.Client
import Network.Socket
import Text.Shakespeare.Text ( st )

data Blob = Blob { blobContent  :: ByteString
                 , blobEncoding :: Text
                 , blobSha      :: Text
                 , blobSize     :: Int } deriving Show

instance FromJSON Blob where
  parseJSON (Object v) = Blob <$> v .: "content"
                              <*> v .: "encoding"
                              <*> v .: "sha"
                              <*> v .: "size"
  parseJSON _ = mzero

gitHubReadBlob :: Text -> Text -> Text -> IO (Maybe Blob)
gitHubReadBlob owner repo sha =
  restfulGet
    [st|https://api.github.com/repos/#{owner}/#{repo}/git/blobs/#{sha}|]

data Content = Content { contentContent  :: ByteString
                       , contentEncoding :: Text } deriving Show

instance FromJSON Content where
  parseJSON (Object v) = Content <$> v .: "content"
                                 <*> v .: "encoding"
  parseJSON _ = mzero

instance ToJSON Content where
  toJSON (Content bs enc) = object ["content" .= bs, "encoding" .= enc]

instance Default Content where
  def = Content B.empty "utf-8"

data Sha = Sha { shaSha :: Text } deriving Show

instance FromJSON Sha where
  parseJSON (Object v) = Sha <$> v .: "sha"
  parseJSON _ = mzero

gitHubWriteBlob :: Text -> Text -> Text -> ByteString -> IO (Maybe Sha)
gitHubWriteBlob token owner repo content =
  restfulPostEx (Content content "utf-8")
    [st|https://api.github.com/repos/#{owner}/#{repo}/git/blobs|] $
    addHeader "Authorization" ("token " <> token)

main :: IO ()
main = withSocketsDo $ do
  -- print =<<
  --   gitHubReadBlob "fpco" "gitlib" "d0047eb9166206e67bb12dbb6f1de65b89d6b68e"
  print =<<
    gitHubWriteBlob "_" "fpco" "gitlib"
      (E.encodeUtf8 "Hello, world!")

-- GitHub.hs
