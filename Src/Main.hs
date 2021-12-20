{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings, TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Relude
import Network.HTTP.Client (parseRequest, Request)
import Network.HTTP.Types
import Network.HTTP.Simple hiding (Proxy(..))
import Data.List (intercalate)
import System.Process (readProcess)
import Conduit (runConduitRes, (.|))
import Data.Conduit.Binary (sinkFile)
import Src.Data
import Data.Aeson (FromJSON)
import Data.Data (Proxy(..))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Cont
import Data.Foldable

data InvidError = NoServiceAvailableError | BadJsonParsingError JSONException | NoVideosError
    deriving (Show)

type Url = String
type VidId = String

instance Monad m => Semigroup (ExceptT e m a) where
    (<>) x y = ExceptT $ do 
        z <- runExceptT x
        case z of
            Right x -> return $ Right x
            Left  _ -> do
                z <- runExceptT y
                case z of
                    Right x -> return $ Right x
                    Left  y -> return $ Left y

foldSemi :: Semigroup a => NonEmpty a -> a
foldSemi (x :| xs) = foldl (<>) x xs

callApi :: FromJSON b => Request -> IO (Either InvidError b)
callApi r = do
  response <- httpJSONEither r -- httpJSONEither
  return $ mapLeft BadJsonParsingError $ getResponseBody response

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left  x) = Left $ f x
mapLeft _ (Right x) = Right x

-- /videos/aqz-KE-bpKQ?fields=videoId,title,description,formatStreams&pretty=1

makeRequest :: Url -> IO Request
makeRequest url =
    setRequestHeader hUserAgent [ "Mozilla/5.0 (Windows NT 10.0; Win64; x64)" ]
    <$> parseRequest url


getJSON :: FromJSON a => Url -> ExceptT InvidError IO a
getJSON url = ExceptT $ do
    request <- makeRequest url
    callApi request
    -- case body of
    --     Left e -> do
    --         print e
    --         pure Nothing
    --     Right x -> pure $ Just x

getVideo :: Instance -> VidId -> ExceptT InvidError IO Video
getVideo inst vidId = getJSON $ uri (info inst) <> "/api/v1/videos/" <> vidId <> "?fields=" <> intercalate "," (getFields @Video Proxy)

getInstances :: ExceptT InvidError IO [Instance]
getInstances = getJSON "https://api.invidious.io/instances.json?sort_by=health"

openMpv :: Maybe Url -> IO ()
openMpv x = case x of
    Just x -> do
        x <- readProcess "mpv" [x] ""
        mempty
    Nothing -> mempty

-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md#streaming
-- https://stackoverflow.com/a/40837552
downloadVideo :: FilePath -> Url -> IO ()
downloadVideo path url = do
    req <- makeRequest url
    runConduitRes $ httpSource req getResponseBody .| sinkFile path

tryInsts :: VidId -> [Instance] -> ExceptT InvidError IO (Instance, Video)
tryInsts vidId insts = case viaNonEmpty foldSemi $ map (\inst -> (inst,) <$> getVideo inst vidId) $ insts of
    Nothing -> ExceptT $ return $ Left NoVideosError
    Just x -> x
-- tryInsts _ [] = undefined
-- tryInsts vidId [inst] = (inst,) <$> getVideo inst vidId
-- tryInsts vidId (inst:rest) = tryInsts vidId [inst] <> tryInsts vidId rest

main :: IO ()
main = do
    runExceptT $ do
        insts <- getInstances
        (inst, x) <- tryInsts "bKxccejvdtU" insts
        lift $ print $ type_ <$> viaNonEmpty last (formatStreams x)
        lift $ print $ encoding <$> viaNonEmpty last (formatStreams x)
        lift $ print $ url <$> viaNonEmpty last (formatStreams x)
        case lift <$> downloadVideo ("Music/" <> title x <> ".mp4") <$> url <$> viaNonEmpty last (formatStreams x) of
            Nothing -> ExceptT $ return $ Left NoVideosError
            Just x -> x
        return Nothing
    mempty
--main :: IO ()
--main = runExceptT $ do
    -- insts <- getInstances
    -- x <- getVideo (head <$> insts) "WHLqPYlNII8"
    -- print $ type_ . last . formatStreams <$> x
    -- print $ encoding . last . formatStreams <$> x
    -- print $ url . last . formatStreams <$> x
    -- downloadVideo "Music/test.mp4" $ url . last . formatStreams <$> x
    --openMpv $ url . last . formatStreams <$> x
