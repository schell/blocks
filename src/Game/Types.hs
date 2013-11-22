module Game.Types where

import           App.Input
import           Graphics.Renderer
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


-- | An instance of a tetris board.
type Board = [Block]


data Tetris = Tetris { _board :: Board
                     , _thisBlock :: Maybe Block
                     , _nextBlocks:: [Block]
                     } deriving (Show, Eq, Ord)


-- | The root of our game data.
data Game = Game { _quit :: Bool   -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input :: Input -- ^ Game input state.
                 , _timeAcc :: Double
                 , _fps   :: Double
                 , _tetris :: Tetris
                 } deriving (Show)

