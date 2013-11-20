{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           Control.Monad
import           Game.Game


main :: IO ()
main = void $ initializeApp newGame >>= startApp

