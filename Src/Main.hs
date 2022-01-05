{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Prelude
import Prelude hiding (last, head, map)
import Control.Monad.Except
import Data.List.NonEmpty
import Data.Conduit.Process

import Src.Data
import Src.Net
import Src.Utils
import Control.Lens

getVideo :: VideoId -> Instance -> ExceptT InvidError IO Video
getVideo id inst = getJSON $ uri (info inst) <> "/api/v1/videos/" <> id <> "?fields=" <> getFieldsApi @Video Proxy

getVideoI :: VideoId -> NonEmpty Instance -> ExceptT InvidError IO (Instance, Video)
getVideoI vidId = foldS . map (\inst -> (inst,) <$> getVideo vidId inst)

getPlaylist :: PlaylistId -> Instance -> ExceptT InvidError IO Playlist
getPlaylist id inst = getJSON $ uri (info inst) <> "/api/v1/playlists/" <> id <> "?fields=" <> getFieldsApi @Playlist Proxy

getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty l

getSearchResults :: String -> Instance -> ExceptT InvidError IO [SearchResult]
getSearchResults query inst = do
    lift $ print $ uri (info inst) <> "/api/v1/search/?q=" <> query <> "?fields=" <> getFieldsApi @SearchResult Proxy
    getJSON $ uri (info inst) <> "/api/v1/search/?q=" <> query <> "?fields=" <> getFieldsApi @SearchResult Proxy

getSearchResultsI :: String -> NonEmpty Instance -> ExceptT InvidError IO (Instance, [SearchResult])
getSearchResultsI query = foldS . map (\inst -> (inst,) <$> getSearchResults query inst)

search :: String -> IO [VideoId]
search query = tryIO $ do
    insts <- getInstances
    (inst, results) <- getSearchResultsI query insts
    return [vid | vid <- (^. field @"videoId") <$> results]

    -- (inst, x) <- getVideoI "bKxccejvdtU" insts
    -- lift $ print $ type_ $ last $ formatStreams x
    -- lift $ print $ encoding $ last $ formatStreams x
    -- lift $ print $ url $ last $ formatStreams x
    -- lift $ downloadChunked ("Music/" <> x ^. (field @"title") <> ".mp4") $ url $ last $ formatStreams x

searchAndPlay :: String -> IO ()
searchAndPlay query = report $ do
    insts <- getInstances
    (inst, results) <- getSearchResultsI query insts
    lift $ print results
    resVids <- failWith NoVideosError $ nonEmpty
        [vid | vid <- (^. field @"videoId") <$> results]
    vid <- getVideo (head resVids) inst
    lift $ print $ url $ last $ formatStreams vid
    lift $ print (vid ^. field @"title")
    lift $ openMpv $ url $ last $ formatStreams vid

openMpv :: Url -> IO ()
openMpv x = void $ readProcess "mpv" [x] ""


main :: IO ()
main = report $ do
    insts <- getInstances
    (inst, x) <- getVideoI "bKxccejvdtU" insts
    lift $ print $ type_ $ last $ formatStreams x
    lift $ print $ encoding $ last $ formatStreams x
    lift $ print $ url $ last $ formatStreams x
    lift $ downloadChunked ("Music/" <> x ^. (field @"title") <> ".mp4") $ url $ last $ formatStreams x

-- replicate this: 
-- youtube-dl --audio-quality 0 -i --extract-audio --audio-format mp3 -o './%(title)s.%(ext)s' --add-metadata --embed-thumbnail --metadata-from-title "%(artist)s - %(title)s" YOUTUBE_LINKS
