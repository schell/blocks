module Game.Game where

import           Game.Block
import           Game.Step
import           Graphics.Renderer
import           App.Input
import           App.TypeClasses
import           App.Clock
import           Graphics.UI.GLFW
import           Math.Matrix
import           Data.Maybe
import           Control.Monad
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix, get )


-- | The root of our game data.
data Game = Game { _quit :: Bool   -- ^ Whether or not the game should quit.
                 , _renderer :: Maybe Renderer -- ^ The renderer.
                 , _input :: Input -- ^ Game input state.
                 , _fps   :: Double
                 , _tetris :: Tetris
                 } deriving (Show)


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               , _fps = 0
               , _tetris = newTetris
               }




instance UserData Game where
    -- | When we start up initialize all our rendering resources.
    onStart g = do rndr <- initRenderer
                   return g { _renderer = Just rndr }

    -- | When we receive input, store it in our game to use later.type
    onInput i game = let game' = game { _input = i }
                         keys  = _keysPressed $ _inputState i
                     in case keys of
                          Key'Escape:_ -> game' { _quit = True }
                          _            -> game'

    -- | Step the game forward.
    onStep clk game = let game' = game { _fps = _avgFPS clk }
                          dt    = _timeNow clk - _timePrev clk
                      in return $ if _quit game'
                                    then game'
                                    else game' { _tetris = stepTetris (_tetris game') dt }

    -- | Render the game.
    onRender g = case _renderer g of
                     Nothing -> return ()
                     Just _  -> renderGame g

    -- | Whether or not our game should quit.
    shouldQuit = _quit
    -- | When quitting, let the user know.
    onQuit game = do putStrLn $ "Average fps: " ++ show (_fps game)
                     putStrLn "Done!"


boardSize :: (GLfloat, GLfloat)
boardSize = (blockWidth*10, blockWidth*20)


boardPos :: Renderer -> (GLfloat, GLfloat)
boardPos r = (x,y)
    where (w,h)   = boardSize
          (sw,sh) = _screenSize r
          x       = sw/2 - w/2
          y       = sh/2 - h/2


renderBlock :: Renderer -> Block -> IO ()
renderBlock r b = do
    let (bx,by) = boardPos r
        (x,y)   = _blockPos b
        w       = blockWidth
        rows    = _blockPieces b
        mat     = identityN 4 :: Matrix GLfloat
        scale'  = scaleMatrix3d w w 1
        tns a a1 = translationMatrix3d a a1 0 :: Matrix GLfloat
        ys      = map (*w) [0,1] :: [GLfloat]
        xs      = map (*w) [0..3] :: [GLfloat]
        zipDo a a1 a2 = zipWithM_ a2 a a1

    zipDo ys rows $ \y' row -> do
        let yy = by + y + y'
        zipDo xs row $ \x' draw -> when draw $ do
            let xx = bx+x+x'
            _updateColor r $ colorForBlock b
            _updateModelview r $ concat $ mat `multiply` tns xx yy `multiply` scale'
            _rndrQuad r


renderBoard :: Renderer -> Board -> IO ()
renderBoard r b = do
    let mat     = identityN 4 :: Matrix GLfloat
        (w,h)   = boardSize
        scale'  = scaleMatrix3d w h 1
        (x,y)   = boardPos r
        trans   = translationMatrix3d x y 0
    -- Render the background.
    _updateModelview r $ concat $ mat `multiply` trans `multiply` scale'
    _updateColor r $ Color4 0.25 0.25 0.25 1.0
    _rndrQuad r
    -- Render the blocks.
    forM_ b $ renderBlock r


renderGame :: Game -> IO ()
renderGame game = do
    when (isJust $ _renderer game) $ do
        let (w,h)  = _windowSize $ _inputState $ _input game
            r      = (fromJust $ _renderer game) { _screenSize = (fromIntegral w, fromIntegral h) }
            pMat   = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat

        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        _updateProjection r $ concat pMat
        renderBoard r $ _board $ _tetris game
        --forM_ params $ \(c, m, r) -> do
        --    _updateColor renderer c
        --    _updateModelview renderer $ concat m
        --    r renderer


