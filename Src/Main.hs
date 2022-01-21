{-# LANGUAGE FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes, OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import qualified Prelude
import Prelude hiding (last, head, map, filter)
import Control.Monad.Except
import Data.List.NonEmpty
import Data.Conduit.Process (readProcess)

import Src.Data
import Src.Net
import Src.Utils
import Src.Invid

import Options.Generic
import Turtle
import Data.List (intercalate)
import Data.Text (pack)
import System.IO.Temp (withTempDirectory)
import Data.Maybe (fromMaybe)
import Data.List.Split
import System.Environment

data Args w = Search (w ::: String  <?> "Query to search for")
            | Download {
                dir  :: w ::: String <!> "." <?> "Directory to download to",
                o :: w ::: String <!> "?title.mp4" <?> "Output name, with wildcards: ?title"
            }
            | Play
            | PlayTemp
            | GetLink
            deriving Generic

instance ParseRecord (Args Wrapped)
deriving instance Show (Args Unwrapped)

openMpv :: MonadIO io => Url -> io ExitCode
openMpv x = shell ("mpv '" <> pack x <> "'") empty

bestQuality :: Video -> ExceptT InvidError IO Url
bestQuality vid = failWith (NoMp4VideoStreams $ vid ^. field @"videoId") $ url . last
    <$> nonEmpty [x | x <- formatStreams vid, x ^. field @"container" == "mp4"]

getVideoId :: ExceptT InvidError IO VideoId
getVideoId = do
    list <- words <$> lift getLine
    vidId <- failWith (BadVideoId "") $ head <$> nonEmpty list
    failWith (BadVideoId vidId) $ if 11 == Prelude.length vidId then Just vidId else Nothing


type WildCard = String

lexWildcard :: WildCard -> String -> [String]
lexWildcard wc = split (dropInitBlank $ dropInnerBlanks $ dropFinalBlank $ onSublist wc)

substWildcards :: Video -> String -> String
substWildcards vid = concatMap (\x -> if x == "?title" then vid ^. field @"title" else x) . lexWildcard "?title"


-- search :: String -> IO [(VideoId, String)]
-- search query = tryIO $ do
--     insts <- getInstances
--     (inst, results) <- getVideoSearchResults query ?>> insts
--     return [vid | vid <- (\v -> (v ^. field @"videoId", v ^. field @"title")) <$> results]

main :: IO ()
main = tryIO $ do
    args <- lift $ unwrapRecord "Invidious search cli"
    insts <- loadInstances
    case (args :: Args Unwrapped) of
        Search q -> do
            (_, results) <- getVideoSearchResults q ?>> insts
            lift $ putStrLn $ unlines $ do
                res <- results
                return $ res ^. field @"videoId" <> " " <> res ^. field @"title"
        Download dir name -> do
            vidId <- getVideoId
            (_, vid) <- getVideo vidId ?>> insts
            bestQuality vid >>= \x -> lift $ downloadChunked (dir <> "/" <> substWildcards vid name) x
        Play -> do
            vidId <- getVideoId
            (_, vid) <- getVideo vidId ?>> insts
            lift $ putStrLn $ "Playing: " <> vid ^. field @"title"
            exitCode <- openMpv =<< bestQuality vid
            lift $ if exitCode == ExitFailure 127 then
                putStrLn "mpv not found, please install it so you can use invicli play!"
            else putStrLn $ "Could not launch mpv, exit code: " <> show exitCode
        PlayTemp -> do
            vidId <- getVideoId
            (_, vid) <- getVideo vidId ?>> insts
            withTempDirectory "/tmp" "invidious_cli" $ \path -> do
                let vidFile = path <> "/" <> vid ^. (field @"title") <> ".mp4"
                bestQuality vid >>= \x -> lift $ downloadChunked vidFile x
                exitCode <- openMpv vidFile
                if exitCode == ExitFailure 127 then
                    lift $ putStrLn "mpv not found, please install it so you can use invicli play!"
                else lift $ putStrLn $ "Could not launch mpv, exit code: " <> show exitCode
        GetLink -> do
            vidId <- getVideoId
            (_, vid) <- getVideo vidId ?>> insts
            lift $ print vid
            bestQuality vid >>= \x -> lift $ putStrLn x

-- Should add (NonEmpty InstUrl) to state monad over ExceptT
-- replicate this: 
-- youtube-dl --audio-quality 0 -i --extract-audio --audio-format mp3 -o './%(title)s.%(ext)s' --add-metadata --embed-thumbnail --metadata-from-title "%(artist)s - %(title)s" YOUTUBE_LINKS
-- Not Working puaacfjEH_U 
