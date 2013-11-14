{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           App.TypeClasses
import           App.Input
import           Renderer
import           Math.Matrix
import           Graphics.UI.GLFW
import           Data.Maybe
import           Control.Monad
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix )


-- | The root of our game data.
data Game = Game { _quit :: Bool -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input :: Input -- ^ Game input state.
                 } deriving (Show)


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               }


main :: IO ()
main = void $ initializeApp newGame >>= startApp


instance UserData Game where
    -- | When we start up initialize all our rendering resources.
    onStart g = do rndr <- initRenderer
                   return g { _renderer = Just rndr }
    -- | When we receive input, store it in our game to use later.
    onInput i game = game { _input = i }
    -- | Step the game forward.
    onStep _ game = let keys  = _keysPressed $ _inputState $ _input game
                    in case keys of
                        Key'Escape:_ -> game { _quit = True }
                        _            -> game
    -- | Render the game.
    onRender game = do when (isJust $ _renderer game) $ do
                           let renderer = fromJust $ _renderer game
                               (w,h) = _windowSize $ _inputState $ _input game
                               pMat  = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
                               mvMat = multiply (identityN 4 :: Matrix GLfloat) $ scaleMatrix3d 100 100 1

                           print (w,h)
                           viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
                           _updateProjection renderer $ concat pMat
                           _updateModelview renderer $ concat mvMat
                           _rndrQuad renderer
                           _rndrTri renderer
                       return game
    -- | Whether or not our game should quit.
    shouldQuit = _quit
    -- | When quitting, let the user know.
    onQuit _ = putStrLn "Done!"


