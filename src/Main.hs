{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           App.TypeClasses
import           App.Input
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW
import           Control.Monad ( unless )
import           System.Exit   ( exitSuccess )
import qualified Data.ByteString as B

main :: IO ()
main = do
    putStrLn "Hello"
    app <- initializeApp $ Game False
    _   <- startApp app
    return ()

data Game = Game { _quit :: Bool } deriving (Show)

instance UserData Game where
    onStart g = do

        -- Vertex shader.
        v <- createShader VertexShader
        shaderSourceBS v $= vertSrc
        compileShader v
        v'Ok <- get $ compileStatus v
        unless v'Ok $ do
            vlog <- get $ shaderInfoLog v
            putStrLn vlog
            exitSuccess

        -- Frag shader.
        f <- createShader FragmentShader
        shaderSourceBS f $= fragSrc
        compileShader f
        f'Ok <- get $ compileStatus f
        unless f'Ok $ do
            flog <- get $ shaderInfoLog f
            putStrLn flog
            exitSuccess

        -- Both in a program!
        p <- createProgram 
        attachShader p v
        attachShader p f
        attribLocation p "position" $= AttribLocation 0 
        linkProgram p
        p'Ok <- get $ linkStatus p
        validateProgram p
        status <- get $ validateStatus p
        unless (p'Ok && status) $ do
            plog <- get $ programInfoLog p
            putStrLn plog 
        
        putStrLn "Done initing resources."        
        return g

    onInput i game = 
        case _keysPressed $ _inputState i of
            Key'Escape:_ -> game { _quit = True }
            _            -> game
    onStep _ game = game 
    onRender = return
    shouldQuit = _quit
    onQuit _ = putStrLn "Done!"


vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
    [ "attribute vec3 position;"
    , "void main () {"
    , "  gl_Position = vec4(position, 1);"
    , "}"
    ] 

fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "void main() {"
    , "  gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);"
    , "}"
    ]


