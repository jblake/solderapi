-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

{-# LANGUAGE RecordWildCards #-}

module API
where

import qualified Codec.Binary.Base16 as Hex
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Digest.MD5
import qualified Data.Map as M
import System.Directory
import System.FilePath
import qualified Text.JSON as JS

import Spec

type MD5Map = M.Map FilePath String
type MD5Cache = StateT MD5Map

mkParent :: (MonadIO m) => FilePath -> m ()
mkParent fp = liftIO $ createDirectoryIfMissing True $ takeDirectory fp

getMD5 :: (MonadIO m) => FilePath -> MD5Cache m String
getMD5 fp = do
  md5 <- gets $ M.lookup fp
  case md5 of
    Just md5' -> return md5'
    Nothing  -> do
      bs <- liftIO $ BS.readFile fp
      let md5' = Hex.encode $ hash $ BS.unpack bs
      liftIO $ putStrLn $ md5' ++ " <- " ++ fp
      modify $ M.insert fp md5'
      return md5'

copyResource :: (MonadIO m) => FilePath -> String -> String -> FilePath -> MD5Cache m String
copyResource outputDir modPack resName fp = do
  md5 <- getMD5 fp
  let fp' = outputDir </> "modpack" </> modPack </> "resources" </> resName
  exist <- liftIO $ doesFileExist fp'
  when (not exist) $ do
    liftIO $ putStrLn $ md5 ++ " -> " ++ fp'
    mkParent fp'
    liftIO $ BS.writeFile fp' =<< BS.readFile fp
  return md5

copyMod :: (MonadIO m) => FilePath -> FilePath -> MD5Cache m String
copyMod outputDir fp = do
  md5 <- getMD5 fp
  let fp' = outputDir </> "mods" </> (md5 ++ ".zip")
  exist <- liftIO $ doesFileExist fp'
  when (not exist) $ do
    liftIO $ putStrLn $ md5 ++ " -> " ++ fp'
    mkParent fp'
    liftIO $ BS.writeFile fp' =<< BS.readFile fp
  return md5

copyJSON :: (JS.JSON v, MonadIO m) => FilePath -> FilePath -> M.Map String v -> m ()
copyJSON outputDir fp js = do
  let fp' = outputDir </> fp </> "index.json"
  liftIO $ putStrLn $ "                            json -> " ++ fp'
  let json = JS.encodeStrict $ JS.toJSObject $ M.assocs $ M.map JS.showJSON js
  mkParent fp'
  liftIO $ writeFile fp' json

mkAPI :: (MonadIO m) => Spec -> m ()
mkAPI (Spec {..}) = do

  names <- evalStateT (forM (M.assocs modPacks) copyModPack) M.empty

  copyJSON outputDir "modpack" $ M.fromList
    [ ("modpacks",   showMap $ M.fromList names)
    , ("mirror_url", JS.showJSON outputURL)
    ]

  copyJSON outputDir "" $ M.fromList
    [ ("api",     "TechnicSolder") -- The name of the API?
    , ("version", "0.3")           -- The version of the API?
    , ("stream",  "jblake")        -- The implementation of the API?
    ]

  where

    copyModPack (modPack, ModPack {..}) = do

      backgroundMD5 <- copyResource outputDir modPack "background.jpg" background
      iconMD5       <- copyResource outputDir modPack "icon.png"       icon
      logoMD5       <- copyResource outputDir modPack "logo_180.png"   logo

      forM_ (M.assocs versions) $ copyModPackVersion modPack

      copyJSON outputDir ("modpack" </> modPack) $ M.fromList
        [ ("name",           JS.showJSON modPack)
        , ("display_name",   JS.showJSON niceName)
        , ("url",            JS.JSNull) -- I guess we could override the resources URL on a per-modpack basis? We don't want to, anyway.
        , ("background_md5", JS.showJSON backgroundMD5)
        , ("icon_md5",       JS.showJSON iconMD5)
        , ("logo_md5",       JS.showJSON logoMD5)
        , ("recommended",    JS.showJSON recVersion)
        , ("latest",         JS.showJSON newVersion)
        , ("builds",         JS.showJSON $ M.keys versions)
        ]

      return (modPack, niceName)

    copyModPackVersion modPack (modPackVersion, ModPackVersion {..}) = do

      -- Note that we do not actually copy the minecraft.jar; we only need it around to get the MD5.
      -- I guess the Platform gets it from an official server or something?
      minecraftMD5 <- getMD5 minecraftJar

      mods <- forM (M.assocs mods) copyModVersion

      copyJSON outputDir ("modpack" </> modPack </> modPackVersion) $ M.fromList
        [ ("minecraft",     JS.showJSON minecraftVersion)
        , ("minecraft_md5", JS.showJSON minecraftMD5)
        , ("forge",         JS.JSNull) -- No idea how this is supposed to work, but the official modpacks set it to null, so that's what I'm doing.
        , ("mods",          JS.showJSON mods)
        ]

    copyModVersion (modName, ModVersion {..}) = do

      md5 <- copyMod outputDir zipFile

      return $ showMap $ M.fromList
        [ ("name",    JS.showJSON modName)
        , ("version", JS.showJSON version)
        , ("md5",     JS.showJSON md5)
        , ("url",     JS.showJSON $ outputURL ++ "/mod/" ++ (md5 ++ ".zip"))
        ]
