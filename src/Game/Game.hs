module Game.Game where

import           Game.Block
import           Game.Tetris
import           Game.Types
import           Game.Events
import           App.Types
import           App.Input
import           App.TypeClasses
import           Graphics.Types
import           Graphics.Renderer
import           Graphics.UI.GLFW
import           Math.Matrix
import           Data.Maybe
import           System.Random
import           Control.Monad
import           Control.Monad.State
import           Control.Lens
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix, get, scale )


defaultOptions :: Options
defaultOptions = Options { _optAssetDir = "./assets" }


makeApp :: UserApp Game
makeApp = UserApp
    -- | When we start up initialize all our rendering resources.
    { _onStart = \g -> do rndr <- initRenderer $ g^.options.optAssetDir
                          return $ g & renderer .~ Just rndr

    -- | When we receive input, store it in our game to use later.type
    , _onInput = \i -> execState $ do input .= i
                                      quit  .= case i^.inputState.keysPressed of
                                                   Key'Escape:_ -> True
                                                   _            -> False

    -- | Step the game forward.
    , _onStep = \clk game -> do let g = execState (setGame clk) game
                                if isNothing $ g^.tetris.block
                                  then setGameWithNextBlock g
                                  else return g

    -- | Render the game.
    , _onRender = \g -> when (isJust $ g^.renderer) $ renderGame g

    -- | Whether or not our game should quit.
    , _shouldQuit = _quit

    -- | When quitting, let the user know.
    , _onQuit = \g -> do putStrLn $ "Average fps: " ++ show (_fps g)
                         putStrLn "Done!"

    , _userData = newGame
    }

-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _renderer = Nothing
               , _input = emptyInput
               , _timeAcc = 0
               , _fps = 0
               , _tetris = newTetris
               , _score = 0
               , _options = defaultOptions
               }


frameRate :: Double
frameRate = 1/30


setGame :: Clock -> State Game ()
setGame clk = do
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

    lc <- use (tetris.lineCount)
    score .=  lc



setGameWithNextBlock :: Game -> IO Game
setGameWithNextBlock g = do
    r <- randomRIO (0, length blockTypes -1)
    let b = newBlockWithType $ blockTypes !! r
    return $ g & tetris.block .~ Just b



boardPos :: Renderer -> (GLfloat, GLfloat)
boardPos r = (x,y)
    where (w,h)   = boardSize
          (sw,sh) = _screenSize r
          x       = sw/2 - w/2
          y       = sh/2 - h/2


renderBlock :: Renderer -> Block -> IO ()
renderBlock r b = do
    let q       = _quadRndr r
        (bx,by) = boardPos r
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

    currentProgram $= (Just $ r^.quadRndr.quadProgram.program)

    zipDo ys rows $ \y' row -> do
        let yy = by + y + y'
        zipDo xs row $ \x' draw -> when draw $ do
            let xx = bx+x+x'
            q^.setQuadColor $ colorForBlock b
            q^.quadProgram.setModelview $ concat $ mat `multiply` tns xx yy `multiply` scale'
            q^.rndrQuad


renderBoard :: Renderer -> Board -> IO ()
renderBoard r b = do
    let q       = _quadRndr r
        mat     = identityN 4 :: Matrix GLfloat
        (w,h)   = boardSize
        scale'  = scaleMatrix3d w h 1
        (x,y)   = boardPos r
        trans   = translationMatrix3d x y 0

    currentProgram $= (Just $ q^.quadProgram.program)

    -- Render the background.
    q^.quadProgram.setModelview $ concat $ mat `multiply` trans `multiply` scale'
    q^.setQuadColor $ Color4 0.25 0.25 0.25 1.0
    q^.rndrQuad
    -- Render the blocks.
    forM_ b $ renderBlock r


renderGame :: Game -> IO ()
renderGame game =
    when (isJust $ _renderer game) $ do
        let (w,h)  = game^.input.inputState.windowSize
            r      = (fromJust $ _renderer game) { _screenSize = (fromIntegral w, fromIntegral h) }
            q      = _quadRndr r
            pMat   = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
            mat    = identityN 4 :: Matrix GLfloat
            scaleb = scaleMatrix3d 16 16 1 :: Matrix GLfloat
            tetris = _tetris game
            block  = _block tetris
            board  = _board tetris
            -- Add the current block to the board if it exists.
            board' = maybe board (:board) block
        -- Update the viewport to match the current window size.
        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))

        currentProgram $= (Just $ q^.quadProgram.program)

        q^.quadProgram.setProjection $ concat pMat
        renderBoard r board'
        when (_gameOver tetris) $ do
            let (w',h') = boardSize
                (x,y)   = boardPos r
                trans   = translationMatrix3d x y 0
                scale   = scaleMatrix3d w' h' 1
            q^.quadProgram.setModelview $ concat $ mat `multiply` trans `multiply` scale
            q^.setQuadColor $ Color4 0.0 0.0 0.0 0.3
            q^.rndrQuad

        -- Example of drawing a string.
        currentProgram $= (Just $ r^.textRndr.textProgram.program)
        r^.textRndr.setSampler $ Index1 0
        r^.textRndr.setTextColor $ Color4 1.0 1.0 1.0 1.0
        r^.textRndr.textProgram.setProjection $ concat pMat
        r^.textRndr.textProgram.setModelview $ concat $ mat `multiply` scaleb
        r^.textRndr.drawText $ "Score: " ++ (show $ game^.score)


