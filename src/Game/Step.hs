module Game.Step where


import Game.Block
import Control.Monad.State
import Graphics.Rendering.OpenGL
import Debug.Trace


-- | An instance of a tetris board.
type Board = [Block]

data Tetris = Tetris { _board :: Board
                     , _thisBlock :: Maybe Block
                     , _nextBlock :: Block
                     } deriving (Show, Eq, Ord)


newTetris :: Tetris
newTetris = Tetris { _board = []
                   , _thisBlock = Just blockI
                   , _nextBlock = blockZ
                   }


fallAmount :: Double -> GLfloat
fallAmount = (30*) . realToFrac -- px/sec


stepTetris :: Tetris -> Double -> Tetris
stepTetris tetris dt = 
    let mB = _thisBlock tetris
    in case mB of
           Nothing -> tetris
           Just b  -> let (x,y)      = _blockPos b
                          board      = _board tetris 
                          (hit, pos) = blockHasHit board b 
                          pos'       = if hit then pos else (x, y + fallAmount dt)
                          b'         = b { _blockPos = pos' }
                          tetris'    = if hit
                                         then tetris { _thisBlock = Nothing
                                                     , _board = b':board
                                                     }
                                         else tetris { _thisBlock = Just b' }

                      in tetris'


blockHasHit :: Board -> Block -> (Bool, Pos)
blockHasHit bo bl =
    let (x,y) = _blockPos bl
        (_,h) = boardSize
    -- Check for going past the y boundary.
    in if y + blockWidth > h 
         then (True, (x, h - blockWidth))
         else (False, (x,y))
