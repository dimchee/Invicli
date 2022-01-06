{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings, TupleSections #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric     #-}

module Main where

import qualified Prelude
import Prelude hiding (last, head, map)
import Control.Monad.Except
import Data.List.NonEmpty
import Data.Conduit.Process (readProcess)

import Src.Data
import Src.Net
import Src.Utils
import Control.Lens

import Options.Generic
import Turtle
import Data.List (intercalate)
import Data.Text (pack)

data Args = Search String
          | Download VideoId
          deriving (Generic, Show)

instance ParseRecord Args

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


search :: String -> IO [VideoId]
search query = tryIO $ do
    insts <- getInstances
    (inst, results) <- getVideoSearchResults query ?>> insts
    return [vid | vid <- (^. field @"videoId") <$> results]

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

main :: IO ()
main = do
    args <- getRecord "Invidious search cli"
    case (args :: Args) of
        Search q -> searchAndPlay q
        _ -> error "Not implemented yet"
-- main = report $ do
--     insts <- getInstances
--     (inst, x) <- getVideo "bKxccejvdtU" ?>> insts
--     lift $ print $ type_ $ last $ formatStreams x
--     lift $ print $ encoding $ last $ formatStreams x
--     lift $ print $ url $ last $ formatStreams x
--     lift $ downloadChunked ("Music/" <> x ^. (field @"title") <> ".mp4") $ url $ last $ formatStreams x

-- replicate this: 
-- youtube-dl --audio-quality 0 -i --extract-audio --audio-format mp3 -o './%(title)s.%(ext)s' --add-metadata --embed-thumbnail --metadata-from-title "%(artist)s - %(title)s" YOUTUBE_LINKS
