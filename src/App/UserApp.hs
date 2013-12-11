module App.UserApp where

import App.Input
import App.Clock

data UserApp a = UserApp
    -- | Starts up the app. Init resources, etc.
    { _onStart :: a -- ^ User's custom data structure.
               -> IO a
    -- | Gives input to the app. Called just before step.
    , _onInput :: Input -- ^ The current input.
               -> a     -- ^ The user's custom data structure.
               -> a
    -- | The main step function.
    , _onStep :: Clock -- ^ The time elapsed since last tick.
              -> a     -- ^ The user's custom data structure.
              -> IO a
    -- | Render the app.
    , _onRender :: a     -- ^ The user's custom data structure.
                -> IO ()
    -- | Cleanup and prepare to exit. Save state, etc.
    , _onQuit :: a     -- ^ User's custom data structure.
              -> IO ()
    -- | Tells when the app should quit. This is called after each step
    -- and if it returns True stepping will cease and onQuit will be called before
    -- exiting the program.
    , _shouldQuit :: a -- ^ User's custom data structure.
                  -> Bool
    -- | The user's app data.
    , _userData :: a
    }

