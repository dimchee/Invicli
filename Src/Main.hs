{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings, TupleSections #-}

module Main where

import qualified Prelude
import Prelude hiding (last, head, map)
import Src.Data
import Src.Net
import Control.Monad.Except
import Data.List.NonEmpty
import Data.Conduit.Process
import Src.Utils
import Data.Proxy
import Data.List (intercalate)

getVideo :: VidId -> Instance -> ExceptT InvidError IO Video
getVideo vidId inst = getJSON $ uri (info inst) <> "/api/v1/videos/" <> vidId <> "?fields=" <> intercalate "," (getFields @Video Proxy)

getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty l

openMpv :: Url -> IO ()
openMpv x = void $ readProcess "mpv" [x] ""


tryInsts :: VidId -> NonEmpty Instance -> ExceptT InvidError IO (Instance, Video)
tryInsts vidId = foldS . map (\inst -> (inst,) <$> getVideo vidId inst)


main :: IO ()
main = report $ do
        insts <- getInstances
        (inst, x) <- tryInsts "bKxccejvdtU" insts
        lift $ print $ type_ $ last $ formatStreams x
        lift $ print $ encoding $ last $ formatStreams x
        lift $ print $ url $ last $ formatStreams x
        lift $ downloadChunked ("Music/" <> title x <> ".mp4") $ url $ last $ formatStreams x
