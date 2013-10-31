-- | Mostly GLFW stuff. 
-- See http://www.glfw.org/docs/3.0/moving.html#moving_window_handle
--
module App.App where

import App.Clock
import App.Input
import App.TypeClasses
import Control.Monad.State
import Data.Maybe
import System.Exit                   ( exitSuccess )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import Graphics.UI.GLFW             hiding ( getTime )

data App a = App { _userData   :: a
                 , _userInput  :: Input
                 , _clock      :: Clock
                 , _window     :: Maybe Window
                 }

initializeApp :: a -> IO (App a)
initializeApp a = do
    Just win <- initGLFW
    time     <- getTime

    -- When True this gives us a moment
    -- to attach an OGL profiler.
    when False $ do
        putStrLn "Waiting for any button press..."
        void getChar
        return ()


    -- Register our resize window function.
    setWindowSizeCallback win $ Just (\w _ _ -> do
        clear [ColorBuffer, DepthBuffer]
        swapBuffers w)

    let clock' = tickClock time emptyClock
    return App{ _userData   = a
              , _clock      = clock'
              , _userInput  = emptyInput
              , _window     = Just win
              }

startApp :: UserData a => App a -> IO (App a)
startApp app = do
    userData' <- onStart $ _userData app
    let app' = app { _userData = userData' }
    stepIO (shouldQuit . _userData) stepApp app' 

-- | Takes a predicate, a stepping function and a
-- state and loops the stepping function over
-- the state while the predicate evaluates false.
stepIO :: (a -> Bool) -- ^ The predicate.
       -> (a -> IO a) -- ^ The stepping function.
       -> a           -- ^ The game state.
       -> IO a
stepIO p g a
    | p a       = return a
    | otherwise = g a >>= stepIO p g

stepApp :: UserData a => App a -> IO (App a)
stepApp app =
    case _window app of
        Just win -> do 
            t      <- getTime
            input' <- getInput win $ _userInput app

            let clock'     = tickClock t $ _clock app
                userData'  = onInput input' $ _userData app
                userData'' = onStep clock' userData'
                app'       = app { _clock     = clock'
                                 , _userInput = input'
                                 , _userData  = userData''
                                 }

            userData''' <- onRender userData''
            swapBuffers win
            when (shouldQuit userData''') $ onQuit userData''
            return $ app' { _userData = userData''' }
        Nothing -> do onQuit $ _userData app
                      void shutdown 
                      return app

initGLFW :: IO (Maybe Window)
initGLFW = do
    putStrLn "Initializing the OpenGL window and context."
    True <- Graphics.UI.GLFW.init
    -- Set our window hints. 
    setWindowHints
    -- Get the prime monitor.
    --mMon  <- getPrimaryMonitor
    -- Create our window.
    mWin <- createWindow 800 600 "App" Nothing Nothing
    when (isJust mWin) $ 
        do let Just w = mWin
           -- Window will show at upper left corner.
           setWindowPos w 0 0
           -- Set the window title.
           setWindowTitle w "Arborgeddon"

           blend     $= Enabled
           blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

    return mWin

setWindowHints :: IO ()
setWindowHints = do
    defaultWindowHints
    windowHint $ WindowHint'DepthBits 1 

shutdown :: IO Bool
shutdown = do
    terminate
    exitSuccess

