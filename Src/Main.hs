{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import qualified Prelude
import Prelude hiding (last, head, map)
import Control.Monad.Except
import Data.List.NonEmpty
import Data.Conduit.Process
import Data.Proxy

import Src.Data
import Src.Net
import Src.Utils

getVideo :: VideoId -> Instance -> ExceptT InvidError IO Video
getVideo id inst = getJSON $ uri (info inst) <> "api/v1/videos/" <> id <> "?fields=" <> getFieldsApi @Video Proxy

getPlaylist :: PlaylistId -> Instance -> ExceptT InvidError IO Playlist
getPlaylist id inst = getJSON $ uri (info inst) <> "api/v1/playlists/" <> id <> "?fields=" <> getFieldsApi @Playlist Proxy

getVideoI :: VideoId -> NonEmpty Instance -> ExceptT InvidError IO (Instance, Video)
getVideoI vidId = foldS . map (\inst -> (inst,) <$> getVideo vidId inst)

getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty l

search :: String -> Instance -> ExceptT InvidError IO [SearchResult]
search query inst = getJSON $ uri (info inst) <> "api/v1/search/?q=" <> query <> "?fields=" <> getFieldsApi @SearchResult Proxy

openMpv :: Url -> IO ()
openMpv x = void $ readProcess "mpv" [x] ""


main :: IO ()
main = report $ do
    insts <- getInstances
    (inst, x) <- getVideoI "bKxccejvdtU" insts
    lift $ print $ type_ $ last $ formatStreams x
    lift $ print $ encoding $ last $ formatStreams x
    lift $ print $ url $ last $ formatStreams x
    lift $ downloadChunked ("Music/" <> title x <> ".mp4") $ url $ last $ formatStreams x
