module Game.Game where

import           Game.Block
import           Game.Tetris
import           Game.Types
import           Game.Events
import           App.Types
import           App.Input
import           App.TypeClasses
import           Graphics.Renderer
import           Graphics.UI.GLFW
import           Math.Matrix
import           Data.Maybe
import           System.Random
import           Control.Monad
import           Control.Monad.State
import           Control.Lens
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix, get, scale )


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               , _timeAcc = 0
               , _fps = 0
               , _tetris = newTetris
               }


frameRate :: Double
frameRate = 1/30


updateGame :: Clock -> State Game ()
updateGame clk = do
    fps .= clk^.avgFPS

    evs <- use (input.inputEvents)
    trs <- use tetris
    acc <- use timeAcc
    -- Update tetris' events.
    tetris .= handleEvents trs evs
    -- Time stepping.
    let dt    = clk^.timeNow - clk^.timePrev
        acc'  = acc + dt
        steps = floor (acc' / frameRate) :: Int

    timeAcc .= acc' - fromIntegral steps * dt
    when (steps >= 1) $ do
        trs <- use tetris
        let states = iterate (`stepTetris` frameRate) trs
        tetris .= (last $ take (steps + 1) states)


updateGameWithNextBlock :: Game -> IO Game
updateGameWithNextBlock g = do
    r <- randomRIO (0, length blockTypes -1)
    let b = newBlockWithType $ blockTypes !! r
    return $ g & tetris.block .~ Just b


instance UserData Game where
    -- | When we start up initialize all our rendering resources.
    onStart g = do rndr <- initRenderer
                   return $ g & renderer .~ Just rndr

    -- | When we receive input, store it in our game to use later.type
    onInput i = execState $ do input .= i
                               quit  .= case i^.inputState.keysPressed of
                                            Key'Escape:_ -> True
                                            _            -> False

    -- | Step the game forward.
    onStep clk game = do let g = execState (updateGame clk) game
                         if isNothing $ g^.tetris.block
                           then updateGameWithNextBlock g
                           else return g

    -- | Render the game.
    onRender g = when (isJust $ g^.renderer) $ renderGame g

    -- | Whether or not our game should quit.
    shouldQuit = _quit

    -- | When quitting, let the user know.
    onQuit game = do putStrLn $ "Average fps: " ++ show (_fps game)
                     putStrLn "Done!"


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
        yl      = fromIntegral $ length rows
        xl      = fromIntegral $ length $ head rows
        ys      = map (*w) [0..yl] :: [GLfloat]
        xs      = map (*w) [0..xl] :: [GLfloat]
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
renderGame game =
    when (isJust $ _renderer game) $ do
        let (w,h)  = _windowSize $ _inputState $ _input game
            r      = (fromJust $ _renderer game) { _screenSize = (fromIntegral w, fromIntegral h) }
            pMat   = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
            tetris = _tetris game
            block  = _block tetris
            board  = _board tetris
            -- Add the current block to the board if it exists.
            board' = maybe board (:board) block

        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        _updateProjection r $ concat pMat
        renderBoard r board'
        when (_gameOver tetris) $ do
            let (w',h') = boardSize
                (x,y)   = boardPos r
                mat     = identityN 4 :: Matrix GLfloat
                trans   = translationMatrix3d x y 0
                scale   = scaleMatrix3d w' h' 1
            _updateModelview r $ concat $ mat `multiply` trans `multiply` scale
            _updateColor r $ Color4 0.0 0.0 0.0 0.3
            _rndrQuad r

