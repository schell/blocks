module Game.Step where


import Game.Block


-- | An instance of a tetris board.
type Board = [Block]

data Tetris = Tetris { _board :: Board
                     , _thisBlock :: Block
                     , _nextBlock :: Block
                     } deriving (Show, Eq, Ord)

newTetris :: Tetris
newTetris = Tetris { _board = [blockZ]
                   , _thisBlock = blockI
                   , _nextBlock = blockZ
                   }


stepTetris :: Tetris -> Double -> Tetris
stepTetris tetris dt = tetris 
