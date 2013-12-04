{-# LANGUAGE TemplateHaskell #-}
module App.Types (
    App(..),
    clock,
    window,

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

import App.App
import App.Clock
import App.Input
import Control.Lens


makeLenses ''App
makeLenses ''Clock
makeLenses ''Input
makeLenses ''InputEvent
makeLenses ''InputState

