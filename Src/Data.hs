{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, ScopedTypeVariables, TypeApplications, OverloadedStrings #-}
module Src.Data where

import GHC.Generics
import Data.Aeson
import Data.Data (Proxy(..))
import Data.Containers (IsMap(omapKeysWith))
import Control.Monad
import qualified Data.Vector as V

-- https://docs.invidious.io/API.md
data Video = Video {
    title :: String,
    description :: String,
    formatStreams :: [FormatStream]
} deriving (Show, Eq, Generic)
instance FromJSON Video

-- https://api.invidious.io/instances
data Instance = Instance {
    name :: String,
    info :: InstanceInfo
} deriving (Show, Eq)

instance FromJSON Instance where
    parseJSON (Array v)
        | V.length v == 2 = do
            x <- parseJSON $ v V.! 0
            y <- parseJSON $ v V.! 1
            return $ Instance x y
        | otherwise = mzero
    parseJSON _ = mzero

data InstanceInfo = InstanceInfo {
    region :: Maybe String,
    uri :: String
} deriving (Show, Eq)

-- Need just there fields
instance FromJSON InstanceInfo where
    parseJSON = withObject "InstanceInfo" $ \o -> do
    reg <- o .: "region"
    uri <- o .: "uri"
    return $ InstanceInfo reg uri

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
    parseJSON = genericParseJSON $ defaultOptions
        { fieldLabelModifier = \x -> case x of
            "type_" -> "type"
            _     -> x
        }
class Fields f where
  repGetFields :: Proxy (f p) -> [String]

getFields :: forall a. (Generic a, Fields (Rep a)) => Proxy a -> [String]
getFields _ = repGetFields @(Rep a) Proxy

instance Fields V1 where repGetFields _ = []
instance Fields U1 where repGetFields _ = []
instance Fields (K1 i c) where repGetFields _ = []
instance (Fields f) => Fields (M1 D c f) where
  repGetFields _ = repGetFields @f Proxy
instance (Fields f) => Fields (M1 C c f) where
  repGetFields _ = repGetFields @f Proxy
instance (Fields f, Selector c) => Fields (M1 S c f) where
  repGetFields _ = [selName (undefined :: t c f a)]
instance (Fields a, Fields b) => Fields (a :*: b) where
  repGetFields _ = repGetFields @a Proxy <> repGetFields @b Proxy

