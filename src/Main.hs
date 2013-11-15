{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           App.TypeClasses
import           App.Input
import           App.Clock
import           Graphics.Renderer
import           Math.Matrix
import           Graphics.UI.GLFW
import           Data.Maybe
import           Control.Monad
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix )


-- | The root of our game data.
data Game = Game { _quit :: Bool   -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input :: Input -- ^ Game input state.
                 , _tick  :: Integer -- ^ The current tick.
                 , _fps   :: Double
                 } deriving (Show)


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               , _tick = 0
               , _fps = 0
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
    onStep clk game = let keys  = _keysPressed $ _inputState $ _input game
                      in case keys of
                          Key'Escape:_ -> game { _quit = True
                                               , _fps = _avgFPS clk
                                               }
                          _            -> game { _tick = 1 + _tick game }
    -- | Render the game.
    onRender game = do when (isJust $ _renderer game) $ do
                           let renderer = fromJust $ _renderer game
                               (w,h)  = _windowSize $ _inputState $ _input game
                               pMat   = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
                               l      = 20 :: GLfloat
                               mat    = multiply (identityN 4 :: Matrix GLfloat) $ scaleMatrix3d l l 1


                           viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
                           _updateProjection renderer $ concat pMat
                           -- Draw an I-piece
                           _updateColor renderer $ Color4 1.0 0.0 0.0 1.0
                           _updateModelview renderer $ concat mat
                           _rndrI renderer
                           -- Draw a J-piece
                           let trans m = translationMatrix3d 0 (l*2.0) 0 `multiply` m
                               mat' = trans mat
                           _updateColor renderer $ Color4 0.0 0.0 1.0 1.0
                           _updateModelview renderer $ concat mat'
                           _rndrJ renderer
                           -- Draw an L-piece
                           let mat'' = trans mat'
                           _updateColor renderer $ Color4 1.0 0.5 0.0 1.0
                           _updateModelview renderer $ concat mat''
                           _rndrL renderer
                           -- Draw an O-piece
                           let mat''' = trans mat''
                           _updateColor renderer $ Color4 1.0 1.0 0.0 1.0
                           _updateModelview renderer $ concat mat'''
                           _rndrO renderer
                           -- Draw an S-piece
                           let mat'''' = trans mat'''
                           _updateColor renderer $ Color4 0.0 1.0 0.0 1.0
                           _updateModelview renderer $ concat mat''''
                           _rndrS renderer
                           -- Draw a T-piece
                           let mat''''' = trans mat''''
                           _updateColor renderer $ Color4 0.5 0.0 0.5 1.0
                           _updateModelview renderer $ concat mat'''''
                           _rndrT renderer
                           -- Draw a Z-piece
                           let mat'''''' = trans mat'''''
                           _updateColor renderer $ Color4 1.0 0.0 0.0 1.0
                           _updateModelview renderer $ concat mat''''''
                           _rndrZ renderer

                       return game
    -- | Whether or not our game should quit.
    shouldQuit = _quit
    -- | When quitting, let the user know.
    onQuit game = do putStrLn $ "Average fps: " ++ show (_fps game)
                     putStrLn "Done!"


