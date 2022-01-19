{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TupleSections #-}

module Src.Invid where

import Prelude hiding (map, head)
import Src.Data
import Src.Net
import Src.Utils
import Data.Maybe (fromMaybe, fromJust)
import System.Environment (lookupEnv, setEnv)
import System.Directory
import Control.Applicative
import Data.Functor
import Data.Time (getCurrentTime)
import Data.Time.Clock


getInstances :: ExceptT InvidError IO (NonEmpty Instance)
getInstances = do
    l <- getJSON "https://api.invidious.io/instances.json?sort_by=health"
    failWith NoInstancesError $ nonEmpty $
        Prelude.filter ( (/= "invidious-us.kavin.rocks") . (^. field @"name")) $ -- strange stuff happens on this instance
        Prelude.filter ( (== "https") . (^. field @"info" . field @"type_")) l

urlStandard :: Url -> Url
urlStandard x = if Prelude.last x == '/' then Prelude.init x else x

getInstUrls :: ExceptT InvidError IO (NonEmpty InstUrl)
getInstUrls = do
    insts <- getInstances
    return $ urlStandard . (^. field @"info" . field @"uri") <$> insts

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
    envInsts <- lift $ readFromCache "instances"
    return $ fromMaybe insts (envInsts >>= nonEmpty . lines)

saveInstances :: NonEmpty InstUrl -> ExceptT InvidError IO ()
saveInstances insts = do
    file <- lift $ getCacheFile "instances"
    lift $ maybe (const empty) writeFile file $ unlines $ toList insts


readFromCache :: String -> IO (Maybe String)
readFromCache name = do
    path <- getCacheFile name
    b <- maybe (return False) doesFileExist path
    if b then do
        lastUpdate <- getModificationTime $ fromJust path
        curTime <- getCurrentTime
        print $ diffUTCTime curTime lastUpdate
        if diffUTCTime curTime lastUpdate > nominalDay then return Nothing
        else Just <$> readFile (fromJust path)
    else return Nothing

-- Took from https://github.com/tldr-pages/tldr-python-client/blob/main/tldr.py#L81
getCacheDir :: IO (Maybe FilePath)
getCacheDir = do
    dir <- lookupEnv "XDG_CACHE_HOME"
        <||> (lookupEnv "HOME" <>.. "/.cache")
        <||> (Just <$> canonicalizePath "~" <>. "/.cache")
        <>.. "/invicli/"
    maybe empty (createDirectoryIfMissing True) dir
    return dir
    where
        a <>. b= (<> b) <$> a
        a <>.. b= (<>. b) <$> a
        a <||> b = do x <- a; y <- b; return $ x <|> y

getCacheFile :: String -> IO (Maybe FilePath)
getCacheFile name = getCacheDir <&> fmap (<> name)


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
