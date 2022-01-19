{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Src.Invid where

import Prelude hiding (map, head)
import Src.Data
import Src.Net
import Src.Utils
import Data.List (delete, intercalate)
import System.Environment (lookupEnv, setEnv)
import Data.Maybe (fromMaybe)
import Data.List.Split (splitOn)


getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty $
        Prelude.filter ( (/= "invidious-us.kavin.rocks") . (^. field @"name")) $ -- strange stuff happens on this instance
        Prelude.filter ( (== "https") . (^. field @"info" . field @"type_")) l

getInstUrls :: ExceptT InvidError IO (NonEmpty InstUrl)
getInstUrls = do
    insts <- getInstances
    return $ (^. field @"info" . field @"uri") <$> insts

getVideo :: VideoId -> InstUrl -> ExceptT InvidError IO Video
getVideo id inst = onError ("can't get video from: " <> inst) $ getJSON $
    fromApi inst ("videos/" <> id) [("fields", getFieldsApi @Video Proxy)]

getPlaylist :: PlaylistId -> InstUrl-> ExceptT InvidError IO Playlist
getPlaylist id inst = getJSON $
    fromApi inst ("playlists/" <> id) [("fields", getFieldsApi @Playlist Proxy)]


getVideoSearchResults :: String -> InstUrl -> ExceptT InvidError IO [VideoSearchResult]
getVideoSearchResults query inst = getJSON $
    fromApi inst "search" [("q", query), ("fields", getFieldsApi @VideoSearchResult Proxy), ("type", "video")]

loadInstances :: ExceptT InvidError IO (NonEmpty InstUrl)
loadInstances = do
    insts <- getInstUrls
    envInsts <- lift $ lookupEnv "INVIDIOUS_INSTANCES"
    return $ fromMaybe insts (envInsts >>= nonEmpty . splitOn ":")

saveInstances :: NonEmpty InstUrl -> ExceptT InvidError IO ()
saveInstances insts = lift $ setEnv "INVIDIOUS_INSTANCES" $ intercalate ":" $ toList insts

(?>>) :: (InstUrl -> ExceptT InvidError IO a) -> NonEmpty InstUrl -> ExceptT InvidError IO (NonEmpty InstUrl, a)
(?>>) getter insts = do
    (inst, x) <- foldS $ map tryInst insts
    saveInstances $ inst :| [i | i <- toList insts, i /= inst]
    return (inst :| [i | i <- toList insts, i /= inst], x)
    where
        tryInst inst = (inst,) <$> getter inst
        foldS (x :| xs) = foldl (<>) x xs


testInstance :: IO Instance
testInstance = do
    x <- runExceptT $ head <$> getInstances
    case x of
        Right x -> return x
        Left _ -> error "No instances available"
