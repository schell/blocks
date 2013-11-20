module Game.Game where

import           Graphics.Renderer
import           App.Input
import           App.TypeClasses
import           App.Clock
import           Graphics.UI.GLFW
import           Math.Matrix
import           Data.Maybe
import           Control.Monad
import           Control.Monad.State
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix, get )


-- | The kind of tetris block.
data BlockType = TI | TJ | TL | TO | TS | TT | TZ deriving (Show, Eq, Ord)


-- | Ax x,y position
type Pos = (Int, Int)


-- | A list of spaces that may be occupied in a block.
type Pieces = [[Bool]]


-- | The block's representation on the board.
data Block = Block BlockType Pos Pieces deriving (Show)


-- | An instance of a tetris board.
type Board = [Block]


-- | The root of our game data.
data Game = Game { _quit :: Bool   -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input :: Input -- ^ Game input state.
                 , _tick  :: Integer -- ^ The current tick.
                 , _fps   :: Double
                 , _board :: Board
                 } deriving (Show)


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               , _tick = 0
               , _fps = 0
               , _board = []
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
    onRender g = case _renderer g of
                     Nothing -> return g
                     Just r  -> renderGame g

    -- | Whether or not our game should quit.
    shouldQuit = _quit
    -- | When quitting, let the user know.
    onQuit game = do putStrLn $ "Average fps: " ++ show (_fps game)
                     putStrLn "Done!"


renderBoard :: Renderer -> Board -> IO ()
renderBoard r b = do
    let mat     = identityN 4 :: Matrix GLfloat
        w       = 20*10
        h       = 20*20
        scale   = scaleMatrix3d w h 1
        (sw,sh) = _screenSize r
        x       = sw/2 - w/2
        y       = sh/2 - h/2
        trans   = translationMatrix3d x y 0
    -- Render the background
    _updateModelview r $ concat $ mat `multiply` trans `multiply` scale
    _updateColor r $ Color4 0.25 0.25 0.25 1.0
    _rndrQuad r


renderGame :: Game -> IO Game
renderGame game = do
    when (isJust $ _renderer game) $ do
        let (w,h)  = _windowSize $ _inputState $ _input game
            r      = (fromJust $ _renderer game) { _screenSize = (fromIntegral w, fromIntegral h) }
            pMat   = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
            l      = 20 :: GLfloat
            mat    = multiply (identityN 4 :: Matrix GLfloat) $ scaleMatrix3d l l 1
            colors = [ Color4 1.0 0.0 0.0 1.0
                     , Color4 0.0 0.0 1.0 1.0
                     , Color4 1.0 0.5 0.0 1.0
                     , Color4 1.0 1.0 0.0 1.0
                     , Color4 0.0 1.0 0.0 1.0
                     , Color4 0.5 0.0 0.5 1.0
                     , Color4 1.0 0.0 0.0 1.0
                     ]
            renders= [ _rndrI
                     , _rndrJ
                     , _rndrL
                     , _rndrO
                     , _rndrS
                     , _rndrT
                     , _rndrZ
                     ]
            (_,mats) = foldl trans (mat,[]) [0..6 :: Int]
            params = zip3 colors mats renders
            trans (m,ms) _  =
                let m' = translationMatrix3d 0 (l*2.0) 0 `multiply` m
                in (m',ms ++ [m])

        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        _updateProjection r $ concat pMat
        renderBoard r $ _board game
        --forM_ params $ \(c, m, r) -> do
        --    _updateColor renderer c
        --    _updateModelview renderer $ concat m
        --    r renderer

    return game

