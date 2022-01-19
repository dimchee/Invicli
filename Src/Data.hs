{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveAnyClass #-}


module Src.Data (
    module Src.Data,
    module GHC.Generics,
    module Data.Generics.Product,
    module Data.Aeson,
    module Data.Proxy,
    module Data.List.NonEmpty,
    module Control.Lens
) where

import GHC.Generics
import Data.Generics.Product
import Data.Aeson
import Data.Proxy
import Data.List.NonEmpty
import Data.List (intercalate)
import qualified Data.Vector as V
import Data.String (IsString)
import Control.Lens ((^.))

type VideoId = String
type PlaylistId = String

fieldModifier :: (Eq p, IsString p) => p -> p
fieldModifier x = case x of
    "type_" -> "type"
    _     -> x

-- https://docs.invidious.io/API.md
data Video = Video {
    title :: String,
    description :: String,
    formatStreams :: NonEmpty FormatStream
} deriving (Show, Eq, Generic, FromJSON)

-- https://api.invidious.io/instances
data Instance = Instance {
    name :: String,
    info :: InstanceInfo
} deriving (Show, Eq, Generic)

instance FromJSON Instance where
    parseJSON (Array v)
        | V.length v == 2 = do
            x <- parseJSON $ v V.! 0
            y <- parseJSON $ v V.! 1
            return $ Instance x y
        | otherwise = fail "Vector of length different than 2"
    parseJSON _ = fail "Not even Vector"

data InstanceInfo = InstanceInfo {
    region :: Maybe String,
    uri :: String,
    type_ :: String
} deriving (Show, Eq, Generic)

-- Need just there fields
instance FromJSON InstanceInfo where
    parseJSON = withObject "InstanceInfo" $ \o -> do
    reg <- o .: "region"
    uri <- o .: "uri"
    type_ <- o .: "type"
    return $ InstanceInfo reg uri type_

data FormatStream = FormatStream {
    url :: String,
    itag :: String,
    type_ :: String,
    quality :: String,
    container :: String,
    encoding :: String,
    qualityLabel :: String,
    resolution :: String,
    size :: String
} deriving (Show, Eq, Generic)
instance FromJSON FormatStream where
    parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = fieldModifier }

data Playlist = Playlist {
    title :: String,
    playlistId :: String,
    author :: String,
    authorId :: String,
    description :: String,
    descriptionHtml :: String,
    videos :: NonEmpty PVideo
} deriving (Show, Eq, Generic, FromJSON)

newtype PVideo = PVideo {
    videoId :: String
} deriving (Show, Eq, Generic, FromJSON)

data VideoSearchResult = VideoSearchResult {
    title :: String,
    videoId :: VideoId
} deriving (Show, Eq, Generic, FromJSON)
-- data SearchResult = SearchResult {
--     videoId :: Maybe VideoId,
--     playlistId :: Maybe PlaylistId
-- } deriving (Show, Eq, Generic, FromJSON)

type Parameter = (String, String)

fromApi :: Instance -> String -> [Parameter] -> String
fromApi inst what params = uri (info inst) <> "/api/v1/" <> what <> "/?" <>
    intercalate "&" [ x <> "=" <> y | (x, y) <- params]


class FieldsApi f where
  repGetFieldsApi :: Proxy (f p) -> String

getFieldsApi :: forall a. (Generic a, FieldsApi (Rep a)) => Proxy a -> String
getFieldsApi _ = repGetFieldsApi @(Rep a) Proxy

instance FieldsApi V1 where repGetFieldsApi _ = ""
instance FieldsApi U1 where repGetFieldsApi _ = ""
instance {-# OVERLAPPABLE #-}(Generic c, FieldsApi (Rep c)) => FieldsApi (Rec0 c) where
    repGetFieldsApi _ = getFieldsApi @c Proxy
instance (FieldsApi f) => FieldsApi (M1 D c f) where
    repGetFieldsApi _ = repGetFieldsApi @f Proxy
instance (FieldsApi f) => FieldsApi (M1 C c f) where
    repGetFieldsApi _ = repGetFieldsApi @f Proxy
instance (FieldsApi f, Selector c) => FieldsApi (M1 S c f) where
    repGetFieldsApi _ = fieldModifier $ selName (undefined :: t c f a) <> getOthers (repGetFieldsApi @f Proxy)
        where getOthers s = if s == "" then "" else "(" <> s <> ")"

instance (FieldsApi a, FieldsApi b) => FieldsApi (a :*: b) where
    repGetFieldsApi _ = repGetFieldsApi @a Proxy <> "," <> repGetFieldsApi @b Proxy

instance {-# OVERLAPPING #-}FieldsApi (Rec0 String) where
    repGetFieldsApi _ = ""
instance {-# OVERLAPPING #-}FieldsApi (Rec0 (Maybe a)) where
    repGetFieldsApi _ = ""
instance {-# OVERLAPPING #-}(FieldsApi (Rep a), Generic a) => FieldsApi (Rec0 (NonEmpty a)) where
    repGetFieldsApi _ = getFieldsApi @a Proxy
instance {-# OVERLAPPING #-}(FieldsApi (Rep a), Generic a) => FieldsApi (Rec0 [a]) where
    repGetFieldsApi _ = getFieldsApi @a Proxy
