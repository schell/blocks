{-# LANGUAGE TemplateHaskell #-}
module App.Types (
    App(..),
    userApp,
    userInput,
    clock,
    window,

    AppVar,

    UserApp(..),
    onStart,
    onInput,
    onStep,
    onRender,
    onQuit,
    shouldQuit,
    userData,

    Clock(..),
    frames,
    avgFPS,
    timeNow,
    timePrev,

    Input(..),
    inputState,
    inputEvents,

    InputEvent(..),

    InputState(..),
    keysPressed,
    mousePosition,
    mouseButtonsPressed,
    windowSize
) where

import App.UserApp
import App.Clock
import App.Input
import Control.Lens
import Control.Concurrent
import Graphics.UI.GLFW hiding ( getTime )


data App a = App { _userApp    :: UserApp a
                 , _userInput  :: Input
                 , _clock      :: Clock
                 , _window     :: Maybe Window
                 }


type AppVar a = MVar (App a)


makeLenses ''App
makeLenses ''UserApp
makeLenses ''Clock
makeLenses ''Input
makeLenses ''InputEvent
makeLenses ''InputState


