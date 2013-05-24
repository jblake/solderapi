-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module Spec
where

import Control.Applicative
import qualified Data.Map as M
import System.FilePath
import Text.JSON

readMap :: (JSON v) => JSValue -> Result (M.Map String v)
readMap j = do
  al <- readJSON j
  return $ M.fromList $ fromJSObject al

showMap :: (JSON v) => M.Map String v -> JSValue
showMap m = JSObject $ toJSObject $ M.assocs $ M.map showJSON m

data Spec = Spec
  { outputDir :: FilePath
  , outputURL :: FilePath
  , modPacks  :: M.Map String ModPack
  }

instance JSON Spec where

  readJSON j = do
    m <- readMap j
    Spec
      <$> readJSON (m M.! "dir")
      <*> readJSON (m M.! "url")
      <*> readMap  (m M.! "packs")

  showJSON (Spec {..}) = showMap $ M.fromList
    [ ("dir",   showJSON outputDir)
    , ("url",   showJSON outputURL)
    , ("packs", showMap  modPacks)
    ]

data ModPack = ModPack
  { niceName   :: String
  , background :: FilePath
  , icon       :: FilePath
  , logo       :: FilePath
  , versions   :: M.Map String ModPackVersion
  , recVersion :: String
  , newVersion :: String
  }

instance JSON ModPack where

  readJSON j = do
    m <- readMap j
    ModPack
      <$> readJSON (m M.! "name")
      <*> readJSON (m M.! "background")
      <*> readJSON (m M.! "icon")
      <*> readJSON (m M.! "logo")
      <*> readMap  (m M.! "versions")
      <*> readJSON (m M.! "recommended")
      <*> readJSON (m M.! "latest")

  showJSON (ModPack {..}) = showMap $ M.fromList
    [ ("name",        showJSON niceName)
    , ("background",  showJSON background)
    , ("icon",        showJSON icon)
    , ("logo",        showJSON logo)
    , ("versions",    showMap  versions)
    , ("recommended", showJSON recVersion)
    , ("latest",      showJSON newVersion)
    ]

data ModPackVersion = ModPackVersion
  { minecraftVersion :: String
  , minecraftJar     :: FilePath
  , mods             :: M.Map String ModVersion
  }

instance JSON ModPackVersion where

  readJSON j = do
    m <- readMap j
    ModPackVersion
      <$> readJSON (m M.! "minecraft")
      <*> readJSON (m M.! "minecraftJar")
      <*> readMap  (m M.! "mods")

  showJSON (ModPackVersion {..}) = showMap $ M.fromList
    [ ("minecraft",    showJSON minecraftVersion)
    , ("minecraftJar", showJSON minecraftJar)
    , ("mods",         showMap  mods)
    ]

data ModVersion = ModVersion
  { version  :: String
  , zipFile  :: FilePath
  }

instance JSON ModVersion where

  readJSON j = do
    m <- readMap j
    ModVersion
      <$> readJSON (m M.! "version")
      <*> readJSON (m M.! "zip")

  showJSON (ModVersion {..}) = showMap $ M.fromList
    [ ("version", showJSON version)
    , ("zip",     showJSON zipFile)
    ]
