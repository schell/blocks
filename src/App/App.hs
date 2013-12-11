-- | Mostly GLFW stuff.
-- See http://www.glfw.org/docs/3.0/moving.html#moving_window_handle
--
module App.App ( App(..)
               , runApp
               ) where


import App.Types
import App.Clock
import App.Input
import App.UserApp
import Control.Monad.State
import Control.Concurrent
import Control.Lens
import Data.Maybe
import System.Exit                   ( exitSuccess, exitFailure )
import Graphics.Rendering.OpenGL    hiding ( Matrix )
import Graphics.UI.GLFW             hiding ( getTime )


runApp :: String -> UserApp a -> IO ()
runApp title userData = initializeApp title userData >>= startWithMVar


initializeApp :: String -> UserApp a -> IO (AppVar a)
initializeApp title uApp = do
    mWin <- initGLFW title
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
                           let input     = flip execState (_userInput app) $ do
                                               inputEvents %= (WindowSizeChangedTo (w,h):)
                                               inputState.windowSize .= (w,h)
                               userData' = (app^.userApp.onInput) input (app^.userApp.userData)
                           renderUserApp win' $ app^.userApp & userData .~ userData')

                       let clock' = tickClock time emptyClock
                           app    = App { _userApp    = uApp
                                        , _userInput  = emptyInput
                                        , _clock      = clock'
                                        , _window     = Just win
                                        }
                       putMVar mvar app
                       return mvar


startWithMVar :: AppVar a -> IO ()
startWithMVar mvar = do
    app   <- takeMVar mvar
    uData <- app^.userApp.onStart $ app^.userApp.userData
    putMVar mvar $ (app & userApp.userData .~ uData)
    iterateWithMVar mvar


-- Iterates stepApp over the state until shouldQuit evaluates false.
iterateWithMVar :: AppVar a
                -> IO ()
iterateWithMVar mvar = iterate'
    where iterate' = do app <- readMVar mvar
                        unless (app^.userApp.shouldQuit $ app^.userApp.userData) $
                            do stepApp mvar
                               iterate'


renderUserApp :: Window -> UserApp a -> IO ()
renderUserApp win uApp = do
    clear [ColorBuffer, DepthBuffer]
    uApp^.onRender $ uApp^.userData
    swapBuffers win


stepApp :: AppVar a -> IO ()
stepApp mvar = do
    app <- readMVar mvar
    case _window app of
        Just win -> do
            t      <- getTime
            input' <- getInput win $ _userInput app

            let clock'     = tickClock t $ _clock app
                userData'  = (app^.userApp.onInput) input' $ app^.userApp.userData
                app'       = app { _clock     = clock'
                                 , _userInput = input'
                                 }

            userData'' <- (app^.userApp.onStep) clock' userData'
            let app'' = app' & userApp.userData .~ userData''
            swapMVar mvar app'' 
            renderUserApp win $ app''^.userApp

            when ((app^.userApp.shouldQuit) userData'') $ 
                (app^.userApp.onQuit) userData''
            return ()

        Nothing -> do putStrLn "Window is nothing, quitting."
                      app^.userApp.onQuit $ app^.userApp.userData
                      void shutdown

initGLFW :: String -> IO (Maybe Window)
initGLFW s = do
    putStrLn "Initializing the OpenGL window and context."
    True <- Graphics.UI.GLFW.init
    -- Set our window hints.
    setWindowHints
    -- Get the prime monitor.
    --mMon  <- getPrimaryMonitor
    -- Create our window.
    mWin <- createWindow 800 600 s Nothing Nothing
    when (isJust mWin) $
        do let Just w = mWin
           -- Window will show at upper left corner.
           setWindowPos w 0 0

    return mWin


setWindowHints :: IO ()
setWindowHints = do
    defaultWindowHints
    --windowHint $ WindowHint'DepthBits 1
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Any


shutdown :: IO Bool
shutdown = do
    terminate
    exitSuccess

