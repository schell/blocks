{-# LANGUAGE TemplateHaskell #-}
module Game.Types where

import           App.Input
import           App.Clock
import           Graphics.Types
import           Graphics.Renderer
import           Graphics.Rendering.OpenGL
import           Control.Lens


-- | Make lenses for app types.


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
makeLenses ''Block


-- | An instance of a tetris board.
type Board = [Block]


data Tetris = Tetris { _board     :: Board
                     , _block     :: Maybe Block
                     , _timer     :: Double
                     , _gameOver  :: Bool
                     , _lineCount :: Int
                     } deriving (Show, Eq, Ord)
makeLenses ''Tetris

data Options = Options { _optAssetDir :: FilePath } deriving (Show)
makeLenses ''Options

-- | The root of our game data.
data Game = Game { _quit     :: Bool -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input    :: Input -- ^ Game input state.
                 , _timeAcc  :: Double
                 , _fps      :: Double
                 , _tetris   :: Tetris
                 , _score    :: Int
                 , _options  :: Options
                 } deriving (Show)
makeLenses ''Game

