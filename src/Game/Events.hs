module Game.Events where

import Game.Types
import Game.Tetris
import App.Input
import Graphics.UI.GLFW


handleEvents :: Tetris -> [InputEvent] -> Tetris
handleEvents t (KeyButtonDown Key'Left:_)  = moveBlockLeft t
handleEvents t (KeyButtonDown Key'Right:_) = moveBlockRight t
handleEvents t (KeyButtonDown Key'Space:_) = rotateBlock t
handleEvents _ (KeyButtonDown Key'R:_) = newTetris
handleEvents t _ = t


