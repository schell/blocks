{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Game.Game where


import           Game.Types
import           Game.Render
import           App.TypeClasses
import           App.Input
import           App.Clock
import           Graphics.Renderer
import           Graphics.UI.GLFW


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               , _tick = 0
               , _fps = 0
               }


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
    onRender = renderGame
    -- | Whether or not our game should quit.
    shouldQuit = _quit
    -- | When quitting, let the user know.
    onQuit game = do putStrLn $ "Average fps: " ++ show (_fps game)
                     putStrLn "Done!"


