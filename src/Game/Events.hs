module Game.Events where

import Game.Types
import Game.Block
import Game.Tetris
import App.Input
import Graphics.UI.GLFW
import Graphics.Rendering.OpenGL
import Debug.Trace


handleEvents :: Tetris -> [InputEvent] -> Tetris
handleEvents t (KeyButtonDown Key'Left:_)  = moveBlockLeft t
handleEvents t (KeyButtonDown Key'Right:_) = moveBlockRight t
handleEvents t (KeyButtonDown Key'Space:_) = rotateBlock t
handleEvents _ (KeyButtonDown Key'R:_) = newTetris
handleEvents t _ = t


