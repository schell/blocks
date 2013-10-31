module App.Input where

import Graphics.UI.GLFW
import Control.Monad

{- Defining -}
data InputState = InputState { _keysPressed         :: [Key]
                             , _mousePosition       :: (Double, Double)
                             , _mouseButtonsPressed :: [MouseButton]
                             , _windowSize          :: (Int, Int)
                             } deriving (Show, Eq )

data InputEvent = KeyButtonDown Key
                | KeyButtonUp Key
                | MouseButtonDown MouseButton
                | MouseButtonUp MouseButton
                | MouseMovedTo (Double, Double)
                | WindowSizeChangedTo (Int, Int)
                deriving (Show, Eq)

data Input = Input { _inputState :: InputState
                   , _inputEvents       :: [InputEvent]
                   }

emptyInput :: Input
emptyInput = Input { _inputEvents = []
                   , _inputState = emptyInputState
                   }

emptyInputState :: InputState
emptyInputState = InputState { _keysPressed = []
                             , _mousePosition = (0,0)
                             , _mouseButtonsPressed = []
                             , _windowSize = (0,0)
                             }

{- Events -}
-- | Returns the current input state and any input events that occurred
-- between the current and previous states.
getInput :: Window   -- ^ The window. 
         -> Input    -- ^ The previous input state.
         -> IO Input -- ^ The new input state and any input events.
getInput win (Input s _) = do
    (keys, buttons, position, (w, h)) <- getCurrentInput win
    let events'     = keyEvents++buttonEvents++moveEvents++winEvents
        keyEvents   = getKeyEventsBetween (_keysPressed s) keys
        buttonEvents= getButtonEventsBetween (_mouseButtonsPressed s) buttons
        moveEvents  = [ MouseMovedTo position | position /= _mousePosition s ]
        winEvents   = if winSize == _windowSize s then [] else [WindowSizeChangedTo winSize]
        winSize     = (fromIntegral w, fromIntegral h)
    return $ Input (InputState keys position buttons winSize) events'

getKeyEventsBetween :: [Key] -> [Key] -> [InputEvent]
getKeyEventsBetween = makeEventsBetween (KeyButtonDown, KeyButtonUp)

getButtonEventsBetween :: [MouseButton] -> [MouseButton] -> [InputEvent]
getButtonEventsBetween = makeEventsBetween (MouseButtonDown, MouseButtonUp)

makeEventsBetween :: Eq a1 => (a1 -> a, a1 -> a) -> [a1] -> [a1] -> [a]
makeEventsBetween (addC, removeC) from to = fmap addC added ++ fmap removeC removed
    where added   = [ x | x <- from++to, x `notElem` from, x `elem` to ]
          removed = [ x | x <- from++to, x `notElem` to, x `elem` from ]

getCurrentInput :: Window -> IO ([Key], [MouseButton], (Double, Double), (Int, Int))
getCurrentInput w = do
    buttons  <- getButtonsPressed w
    position <- getCursorPos w
    keys     <- getKeysPressed w
    winSize  <- getWindowSize w
    return (keys, buttons, position, winSize)

getButtonsPressed :: Window -> IO [MouseButton]
getButtonsPressed w = listOfPolledInputPressed (mouseButtonIsPressed w) mouseButtonsUsed

mouseButtonIsPressed :: Window -> MouseButton -> IO Bool
mouseButtonIsPressed w = fmap (==MouseButtonState'Pressed) . getMouseButton w

getKeysPressed :: Window -> IO [Key]
getKeysPressed w = listOfPolledInputPressed (keyIsPressed w) keysUsed

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed w = fmap (\k -> k == KeyState'Pressed || k == KeyState'Repeating) . getKey w

listOfPolledInputPressed :: Show a => (a -> IO Bool) -> [a] -> IO [a]
listOfPolledInputPressed fPoll = foldM (\acc a -> do
    isPressed <- fPoll a
    return $ if isPressed
             then a:acc
             else acc) []

keysUsed :: [Key]
keysUsed = addKey Key'Space []
    where addKey Key'Menu ks = Key'Menu:ks
          addKey key      ks = addKey (succ key) (key:ks)

mouseButtonsUsed :: [MouseButton]
mouseButtonsUsed = [ MouseButton'1 ]

