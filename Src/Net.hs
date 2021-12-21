{-# LANGUAGE OverloadedStrings #-}
module Src.Net where

import Src.Utils
import Data.Aeson
import Control.Monad.Except
import Network.HTTP.Client
import Network.HTTP.Simple
import Network.HTTP.Types
import Data.Conduit (runConduitRes, (.|))
import Data.Conduit.Binary (sinkFile)

data InvidError = NoServiceAvailableError
    | NoInstancesError
    | NoVideosError
    | BadJsonParsingError JSONException
    deriving (Show)

type Url = String

makeRequest :: Url -> IO Request
makeRequest url =
    setRequestHeader hUserAgent [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" ]
    <$> parseRequest url

getJSON :: FromJSON a => Url -> ExceptT InvidError IO a
getJSON url = ExceptT $ do
    request <- makeRequest url
    response <- httpJSONEither request
    return $ mapLeft BadJsonParsingError $ getResponseBody response

-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md#streaming
-- https://stackoverflow.com/a/40837552
downloadChunked :: FilePath -> Url -> IO ()
downloadChunked path url = do
    req <- makeRequest url
    runConduitRes $ httpSource req getResponseBody .| sinkFile path
