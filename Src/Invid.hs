{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Src.Invid where

import Prelude hiding (map, head)
import Src.Data
import Src.Net
import Src.Utils

getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty $
        Prelude.filter ( (/= "invidious-us.kavin.rocks") . (^. field @"name")) $ -- strange stuff happens on this instance
        Prelude.filter ( (== "https") . (^. field @"info" . field @"type_")) l

getVideo :: VideoId -> Instance -> ExceptT InvidError IO Video
getVideo id inst = onError ("can't get video from: " <> (inst ^. field @"name")) $ getJSON $
    fromApi inst ("videos/" <> id) [("fields", getFieldsApi @Video Proxy)]

getPlaylist :: PlaylistId -> Instance -> ExceptT InvidError IO Playlist
getPlaylist id inst = getJSON $
    fromApi inst ("playlists/" <> id) [("fields", getFieldsApi @Playlist Proxy)]


getVideoSearchResults :: String -> Instance -> ExceptT InvidError IO [VideoSearchResult]
getVideoSearchResults query inst = getJSON $
    fromApi inst "search" [("q", query), ("fields", getFieldsApi @VideoSearchResult Proxy), ("type", "video")]

search :: String -> IO [(VideoId, String)]
search query = tryIO $ do
    insts <- getInstances
    (inst, results) <- getVideoSearchResults query ?>> insts
    return [vid | vid <- (\v -> (v ^. field @"videoId", v ^. field @"title")) <$> results]

(?>>) :: (Instance -> ExceptT InvidError IO a) -> NonEmpty Instance -> ExceptT InvidError IO (Instance, a)
(?>>) getter = foldS . map (\inst -> (inst,) <$> getter inst)

testInstance :: IO Instance
testInstance = do
    x <- runExceptT $ head <$> getInstances
    case x of
        Right x -> return x
        Left _ -> error "No instances available"
