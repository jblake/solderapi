-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Applicative
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.Posix.Files
import Text.JSON

import API
import Spec

main :: IO ()
main = do

  [specFile, outputDir, outputURL] <- getArgs

  isFile <- doesFileExist specFile
  isDir <- doesDirectoryExist specFile

  if isFile
    then do

      modPacksS <- readFile specFile
      let modPacks = decodeStrict modPacksS >>= readMap

      case modPacks of
        Error m -> putStrLn $ "Error parsing " ++ specFile ++ ": " ++ m
        Ok mps -> mkAPI $ Spec outputDir outputURL mps

    else if isDir
      then do

        mps <- getModPacks specFile

        mkAPI $ Spec outputDir outputURL mps

      else putStrLn $ specFile ++ " does not exist!"

getModPacks :: FilePath -> IO (M.Map String ModPack)
getModPacks dir = do

  packs <- filter (not . isPrefixOf ".") <$> getDirectoryContents dir

  M.fromList <$> forM packs (getModPack dir)

getModPack :: FilePath -> FilePath -> IO (String, ModPack)
getModPack dir mp = do

  name <- head <$> lines <$> readFile (dir </> mp </> "name.txt")

  latest <- getVersion dir mp "latest"
  recommended <- getVersion dir mp "recommended"

  versions <- filter (/= "recommended") <$> filter (/= "latest") <$> filter (not . isPrefixOf ".") <$> getDirectoryContents (dir </> mp)

  mpvs <- M.fromList <$> catMaybes <$> forM versions (getModPackVersion dir mp)

  return (mp, ModPack
    { niceName = name
    , background = dir </> mp </> "background.jpg"
    , icon = dir </> mp </> "icon.png"
    , logo = dir </> mp </> "logo.png"
    , versions = mpvs
    , recVersion = recommended
    , newVersion = latest
    })

getVersion :: FilePath -> FilePath -> FilePath -> IO String
getVersion dir mp v = do

  isFile <- doesFileExist $ dir </> mp </> v

  if isFile
    then head <$> lines <$> readFile (dir </> mp </> v)
    else do

      v <- makeRelative (dir </> mp) <$> readSymbolicLink (dir </> mp </> v)

      if null $ filter (== '/') v
        then return v
        else fail $ "Symbolic link " ++ (dir </> mp </> v) ++ " points outside " ++ (dir </> mp)

getModPackVersion :: FilePath -> FilePath -> FilePath -> IO (Maybe (String, ModPackVersion))
getModPackVersion dir mp mpv = do

  isDir <- doesDirectoryExist $ dir </> mp </> mpv

  if not isDir
    then return Nothing
    else Just <$> getModPackVersion' dir mp mpv

getModPackVersion' :: FilePath -> FilePath -> FilePath -> IO (String, ModPackVersion)
getModPackVersion' dir mp mpv = do

  minecraft <- head <$> lines <$> readFile (dir </> mp </> mpv </> "minecraft.txt")

  files <- filter (not . isPrefixOf ".") <$> getDirectoryContents (dir </> mp </> mpv)

  return (mpv, ModPackVersion
    { minecraftVersion = minecraft
    , minecraftJar = dir </> mp </> mpv </> "minecraft.jar"
    , mods = M.fromList $ catMaybes $ map (parseMod dir mp mpv) files
    })

parseMod :: FilePath -> FilePath -> FilePath -> FilePath -> Maybe (String, ModVersion)
parseMod dir mp mpv fp = case splitOn "_" fp of
  [modName, versionDotZip] ->
    let
      vSplit = splitOn "." versionDotZip
      version = init vSplit
      zip = last vSplit
    in case zip of
      "zip" -> Just (modName, ModVersion (intercalate "." version) (dir </> mp </> mpv </> fp))
      _ -> Nothing
  _ -> Nothing
