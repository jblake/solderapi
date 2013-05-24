-- Copyright © 2013 Julian Blake Kongslie <jblake@omgwallhack.org>
-- Licensed under the MIT license.

module Main
where

import Control.Monad
import System.Environment
import Text.JSON

import API
import Spec

main :: IO ()
main = do

  specFiles <- getArgs

  forM_ specFiles $ \specFile -> do
    putStrLn $ "Processing " ++ specFile

    specS <- readFile specFile
    let spec = decodeStrict specS

    case spec of
      Ok s -> mkAPI s
      Error m -> putStrLn $ "Error: " ++ m
