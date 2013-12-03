{-# LANGUAGE OverloadedStrings #-}
module Main where

import App.App
import Game.Game


main :: IO ()
main = runApp newGame

