module Game.Block where

import           Graphics.Rendering.OpenGL


-- | The kind of tetris block.
data BlockType = TI | TJ | TL | TO | TS | TT | TZ deriving (Show, Eq, Ord)


-- | Ax x,y position
type Pos = (GLfloat, GLfloat)


-- | Two rows of spaces that may be occupied in a block.
type Pieces = [[Bool]] 


-- | The block's representation on the board.
data Block = Block { _blockType   :: BlockType
                   , _blockPos    :: Pos
                   , _blockPieces :: Pieces
                   } deriving (Eq, Ord, Show)


colorForBlock :: Block -> Color4 GLfloat
colorForBlock b = c 
    where Just c = lookup (_blockType b) table
          table  = zip types colors 
          types  = [TI,TJ,TL,TO,TS,TT,TZ] 
          colors = [ Color4 0.0 1.0 1.0 1.0
                   , Color4 0.0 0.0 1.0 1.0
                   , Color4 1.0 0.5 0.0 1.0
                   , Color4 1.0 1.0 0.0 1.0
                   , Color4 0.0 1.0 0.0 1.0
                   , Color4 0.5 0.0 0.5 1.0
                   , Color4 1.0 0.0 0.0 1.0
                   ]


blockWidth :: GLfloat
blockWidth = 20


blockI :: Block
blockI = Block TI (0,0) [row1,row2] 
    where row1 = [True, True, True, True]
          row2 = [False, False, False, False]


blockZ :: Block
blockZ = Block TZ (0,0) [row1, row2]
    where row1 = [True, True, False, False]
          row2 = [False, True, True, False]


