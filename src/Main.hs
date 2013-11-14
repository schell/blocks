{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           App.TypeClasses
import           App.Input
import           Resources
import           Utils
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW
import           Data.Maybe
import           Control.Monad

-- | The root of our game data.
data Game = Game { _quit     :: Bool -- ^ Whether or not the game should quit.
                 , _geometry :: Maybe VertexArrayObject -- ^ A graphic resource.
                 , _keys     :: [Key] -- ^ Current key state.
                 } deriving (Show)


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _geometry = Nothing
               , _keys = []
               }


main :: IO ()
main = void $ initializeApp newGame >>= startApp


instance UserData Game where
    onStart g = do vao <- initResources 
                   return g { _geometry = Just vao }

    onInput i game =
        let keys  = _keysPressed $ _inputState i
            game' = game { _keys = keys }
        in case keys of
            Key'Escape:_ -> game' { _quit = True }
            _            -> game'

    onStep _ game = game

    onRender game = do when (isJust $ _geometry game) $ do
                           drawArrays Triangles 0 3
                           printError
                       return game

    shouldQuit = _quit
    onQuit _ = putStrLn "Done!"




