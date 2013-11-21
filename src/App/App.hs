-- | Mostly GLFW stuff.
-- See http://www.glfw.org/docs/3.0/moving.html#moving_window_handle
--
module App.App ( App(..)
               , runApp
               ) where


import App.Clock
import App.Input
import App.TypeClasses
import Control.Monad.State
import Control.Concurrent
import Data.Maybe
import System.Exit                   ( exitSuccess, exitFailure )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import Graphics.UI.GLFW             hiding ( getTime )


data App a = App { _userData   :: a
                 , _userInput  :: Input
                 , _clock      :: Clock
                 , _window     :: Maybe Window
                 }


type AppVar a = MVar (App a)


runApp :: UserData a => a -> IO ()
runApp userData = initializeApp userData >>= startWithMVar


initializeApp :: UserData a => a -> IO (AppVar a)
initializeApp userData = do
    mWin <- initGLFW
    case mWin of
        Nothing  -> do putStrLn "Could not create a window."
                       exitFailure

        Just win -> do makeContextCurrent mWin
                       time <- getTime
                       mvar <- newEmptyMVar

                       -- When True this gives us a moment
                       -- to attach an OGL profiler.
                       when False $ do
                           putStrLn "Waiting for any button press..."
                           void getChar
                           return ()


                       -- Register our resize window function.
                       setWindowSizeCallback win $ Just (\win' w h -> do
                           app <- readMVar mvar
                           let input     = _userInput app
                               inState   = _inputState input
                               events    = _inputEvents input
                               input'    = input { _inputEvents = WindowSizeChangedTo (w,h):events
                                                 , _inputState = inState { _windowSize = (w,h) }
                                                 }
                               userData' = onInput input' $ _userData app
                           renderUserData win' userData')

                       let clock' = tickClock time emptyClock
                           app    = App { _userData   = userData
                                        , _clock      = clock'
                                        , _userInput  = emptyInput
                                        , _window     = Just win
                                        }
                       putMVar mvar app
                       return mvar


startWithMVar :: UserData a => AppVar a -> IO ()
startWithMVar mvar = do
    app <- takeMVar mvar
    userData' <- onStart $ _userData app
    putMVar mvar $ app { _userData = userData' }
    iterateWithMVar mvar


-- Iterates stepApp over the state until shouldQuit evaluates false.
iterateWithMVar :: UserData a
           => AppVar a
           -> IO ()
iterateWithMVar mvar = iterate'
    where iterate' = do app <- readMVar mvar
                        unless (shouldQuit $ _userData app) $
                            do stepApp mvar
                               iterate'


renderUserData :: UserData a => Window -> a -> IO ()
renderUserData win userData = do
    clear [ColorBuffer, DepthBuffer]
    onRender userData
    swapBuffers win


stepApp :: UserData a => AppVar a -> IO ()
stepApp mvar = do
    app <- readMVar mvar
    case _window app of
        Just win -> do
            t      <- getTime
            input' <- getInput win $ _userInput app

            let clock'     = tickClock t $ _clock app
                userData'  = onInput input' $ _userData app
                app'       = app { _clock     = clock'
                                 , _userInput = input'
                                 }

            userData'' <- onStep clock' userData'
            swapMVar mvar $ app' { _userData = userData'' }

            renderUserData win userData''

            when (shouldQuit userData'') $ onQuit userData''
            return ()

        Nothing -> do putStrLn "Window is nothing, quitting."
                      onQuit $ _userData app
                      void shutdown

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
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Any


shutdown :: IO Bool
shutdown = do
    terminate
    exitSuccess

