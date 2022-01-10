{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import qualified Prelude
import Prelude hiding (last, head, map)
import Control.Monad.Except
import Data.List.NonEmpty
import Data.Conduit.Process (readProcess)

import Src.Data
import Src.Net
import Src.Utils
import Control.Lens ((^.))

import Options.Generic
import Turtle
import Data.List (intercalate)
import Data.Text (pack)

data Args w = Search (w ::: String  <?> "Search for a video, and return list of results <videoId, title>")
          | Download (w ::: VideoId <?> "Download a videoId") (w ::: String <?> "to directory")
          | Play     (w ::: VideoId <?> "Play a videoId using mpv")
          deriving Generic

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

getVideo :: VideoId -> Instance -> ExceptT InvidError IO Video
getVideo id inst = getJSON $
    fromApi inst ("videos/" <> id) [("fields", getFieldsApi @Video Proxy)]

getPlaylist :: PlaylistId -> Instance -> ExceptT InvidError IO Playlist
getPlaylist id inst = getJSON $
    fromApi inst ("playlists/" <> id) [("fields", getFieldsApi @Playlist Proxy)]

getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty l


testInstance :: IO Instance
testInstance = do
    x <- runExceptT $ head <$> getInstances
    case x of
        Right x -> return x
        Left _ -> error "No instances available"

getVideoSearchResults :: String -> Instance -> ExceptT InvidError IO [VideoSearchResult]
getVideoSearchResults query inst = getJSON $
    fromApi inst "search" [("q", query), ("fields", getFieldsApi @VideoSearchResult Proxy), ("type", "video")]

(?>>) :: (Instance -> ExceptT InvidError IO a) -> NonEmpty Instance -> ExceptT InvidError IO (Instance, a)
(?>>) getter = foldS . map (\inst -> (inst,) <$> getter inst)


search :: String -> IO [(VideoId, String)]
search query = tryIO $ do
    insts <- getInstances
    (inst, results) <- getVideoSearchResults query ?>> insts
    return [vid | vid <- (\v -> (v ^. field @"videoId", v ^. field @"title")) <$> results]

searchAndPlay :: String -> IO ()
searchAndPlay query = report $ do
    insts <- getInstances
    (inst, results) <- getVideoSearchResults query ?>> insts
    lift $ print results
    resVids <- failWith NoVideosError $ nonEmpty
        [vid | vid <- (^. field @"videoId") <$> results]
    vid <- getVideo (head resVids) inst
    lift $ print $ url $ last $ formatStreams vid
    lift $ print (vid ^. field @"title")
    lift $ openMpv $ url $ last $ formatStreams vid

openMpv :: Url -> IO ()
openMpv x = void $ readProcess "mpv" [x] ""

choose :: MonadIO io => [String] -> io ExitCode
choose l = shell "fzf" $ foldl1 (<|>) (return <$> textToLines (pack $ unlines l))
-- https://github.com/junegunn/fzf/issues/1660

bestQuality = url . last . formatStreams 

main :: IO ()
main = tryIO $ do
    args <- lift $ unwrapRecord "Invidious search cli"
    insts <- getInstances
    case (args :: Args Unwrapped) of
        Search q -> do
            (_, results) <- getVideoSearchResults q ?>> insts
            lift $ putStrLn $ unlines $ do
                res <- results
                return $ res ^. field @"videoId" <> " " <> res ^. field @"title"
        Download vidId path -> do
            (_, vid) <- getVideo vidId ?>> insts
            lift $ downloadChunked (path <> "/" <> vid ^. (field @"title") <> ".mp4") $ bestQuality $ vid
        Play vidId -> do
            (_, vid) <- getVideo vidId ?>> insts
            lift $ openMpv $ bestQuality $ vid
        _ -> error "Not implemented yet"

-- replicate this: 
-- youtube-dl --audio-quality 0 -i --extract-audio --audio-format mp3 -o './%(title)s.%(ext)s' --add-metadata --embed-thumbnail --metadata-from-title "%(artist)s - %(title)s" YOUTUBE_LINKS
