{-# LANGUAGE OverloadedStrings #-}
module Main where

import App.App
import App.Types
import Game.Game
import Game.Types
import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute mainOptions args of
        ( opts, _,   []) -> runApp "Tetris" $ makeApp {_userData = newGame {_options = gatherOptions opts}}
        (    _, _, msgs) -> error $ concat msgs ++ usageInfo header mainOptions


mainOptions :: [OptDescr (Options -> Options)]
mainOptions = [ Option "a" ["asset directory"]
                  (OptArg updateAssetDir "filepath")
                  "Set the asset directory from which the game pulls assets."
              ]


gatherOptions = foldl (flip id) defaultOptions


header :: String
header = "Usage: blocks [-a filepath]"


updateAssetDir :: Maybe FilePath -> Options -> Options
updateAssetDir mfp opts =
    if isJust mfp
      then opts { _optAssetDir = fromJust mfp }
      else opts


