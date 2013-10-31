module App.TypeClasses where

import App.Input
import App.Clock

class UserData a where
    -- | Starts up the app. Init resources, etc.
    onStart :: a -- ^ User's custom data structure.
            -> IO a
    -- | Gives input to the app. Called just before step.
    onInput :: Input -- ^ The current input.
            -> a     -- ^ The user's custom data structure.
            -> a
    -- | The main step function.
    onStep :: Clock -- ^ The time elapsed since last tick.
           -> a     -- ^ The user's custom data structure.
           -> a
    -- | Render the app.
    onRender :: a     -- ^ The user's custom data structure.
             -> IO a
    -- | Cleanup and prepare to exit. Save state, etc.
    onQuit :: a     -- ^ User's custom data structure.
           -> IO ()
    -- | Tells when the app should quit. This is called after each step
    -- and if it returns True stepping will cease and onQuit will be called before
    -- exiting the program.
    shouldQuit :: a -- ^ User's custom data structure.
               -> Bool


