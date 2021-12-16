{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Data.Aeson (FromJSON, Value (String))
import GHC.Generics (Generic, U1, S1, (:*:), M1)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Client (parseRequest, Request)
import Network.HTTP.Types (hAccept, hAcceptLanguage, hUserAgent)
import Network.HTTP.Simple (JSONException, httpJSONEither, getResponseBody, httpBS, setRequestHeader)
import Data.Data (constrFields, Data (toConstr), dataTypeConstrs, dataTypeOf)
import Data.List (intercalate)
import Distribution.Compat.Lens
import System.Process (readProcess)

-- https://docs.invidious.io/API.md
data Video = Video {
    title :: String,
    description :: String,
    formatStreams :: [FormatStream]
} deriving (Show, Eq, Generic, Data)
instance FromJSON Video

data FormatStream = FormatStream {
    url :: String,
    itag :: String,
    --type :: String,
    quality :: String,
    container :: String,
    encoding :: String,
    qualityLabel :: String,
    resolution :: String,
    size :: String
} deriving (Show, Eq, Generic, Data)
instance FromJSON FormatStream


getFields :: Data a => a -> [String]
getFields = concatMap constrFields . dataTypeConstrs . dataTypeOf


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
fetchVideo vidId = getUpcoming $ baseUrl <> "videos/" <> vidId <> "?fields=" <> intercalate "," (getFields (undefined :: Video))
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
