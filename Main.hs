{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications #-}

import GHC.Generics
import Data.Data (Proxy(..))
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Client (parseRequest, Request)
import Network.HTTP.Types (hAccept, hAcceptLanguage, hUserAgent)
import Network.HTTP.Simple (JSONException, httpJSONEither, getResponseBody, httpBS, setRequestHeader)
import Data.List (intercalate)
import System.Process (readProcess)

-- https://docs.invidious.io/API.md
data Video = Video {
    title :: String,
    description :: String,
    formatStreams :: [FormatStream]
} deriving (Show, Eq, Generic)
instance FromJSON Video

data FormatStream = FormatStream {
    url :: String,
    itag :: String,
    type_ :: String,
    quality :: String,
    container :: String,
    encoding :: String,
    qualityLabel :: String,
    resolution :: String,
    size :: String
} deriving (Show, Eq, Generic)
instance FromJSON FormatStream where
    parseJSON = genericParseJSON $ defaultOptions
        { fieldLabelModifier = \x -> case x of
            "type_" -> "type"
            _     -> x
        }

class Fields f where
  repGetFields :: Proxy (f p) -> [String]

getFields :: forall a. (Generic a, Fields (Rep a)) => Proxy a -> [String]
getFields _ = repGetFields @(Rep a) Proxy

instance Fields V1 where repGetFields _ = []
instance Fields U1 where repGetFields _ = []
instance Fields (K1 i c) where repGetFields _ = []
instance (Fields f) => Fields (M1 D c f) where
  repGetFields _ = repGetFields @f Proxy
instance (Fields f) => Fields (M1 C c f) where
  repGetFields _ = repGetFields @f Proxy
instance (Fields f, Selector c) => Fields (M1 S c f) where
  repGetFields _ = [selName (undefined :: t c f a)]
instance (Fields a, Fields b) => Fields (a :*: b) where
  repGetFields _ = repGetFields @a Proxy <> repGetFields @b Proxy


callApi :: FromJSON b => Request -> IO (Either JSONException b)
callApi r = do
  response <- httpJSONEither r -- httpJSONEither
  pure $ getResponseBody response

-- /videos/aqz-KE-bpKQ?fields=videoId,title,description,formatStreams&pretty=1

makeRequest :: String -> IO Request
makeRequest url =
    setRequestHeader hUserAgent [ S8.pack "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" ]
    <$> parseRequest url

getUpcoming :: String -> IO (Maybe Video)
getUpcoming url = do
    request <- makeRequest url
    body <- callApi request
    case body of
        Left e -> do
            print e
            pure Nothing
        Right x -> pure $ Just x

fetchVideo :: String -> IO (Maybe Video)
fetchVideo vidId = getUpcoming $ baseUrl <> "videos/" <> vidId <> "?fields=" <> intercalate "," (getFields @Video Proxy)
    where baseUrl = "https://invidious.snopyta.org/api/v1/"

openMpv :: Maybe String -> IO ()
openMpv x = case x of
    Just x -> do
        x <- readProcess "mpv" [x] ""
        mempty
    Nothing -> mempty


main :: IO ()
main = do
    x <- fetchVideo "aqz-KE-bpKQ"
    openMpv $ url . last . formatStreams <$> x
