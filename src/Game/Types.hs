module Game.Types where

import           App.Input
import           Graphics.Renderer


-- | The root of our game data.
data Game = Game { _quit :: Bool   -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input :: Input -- ^ Game input state.
                 , _tick  :: Integer -- ^ The current tick.
                 , _fps   :: Double
                 } deriving (Show)

