{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           App.TypeClasses
import           App.Input
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW
import           Control.Monad          ( unless )
import           System.Exit            ( exitFailure )
import           Foreign.Storable       ( sizeOf )
import           Foreign.Marshal.Array  ( withArray )
import           Foreign.Ptr            ( nullPtr )
import           System.IO              ( hPutStrLn, stderr )
import qualified Data.ByteString as B

main :: IO ()
main = do
    putStrLn "Hello"
    app <- initializeApp Game { _quit = False
                              , _geometry = Nothing
                              , _keys = []
                              }
    _   <- startApp app
    return ()

data Game = Game { _quit     :: Bool
                 , _geometry :: Maybe BufferObject
                 , _keys     :: [Key]
                 } deriving (Show)

instance UserData Game where
    onStart g = do

        -- Vertex shader.
        putStrLn "Compiling the vertex shader."
        v <- createShader VertexShader
        shaderSourceBS v $= vertSrc
        compileShader v
        v'Ok <- get $ compileStatus v
        unless v'Ok $ do
            vlog <- get $ shaderInfoLog v
            putStrLn $ "Log:" ++ vlog
            exitFailure

        printError

        -- Frag shader.
        putStrLn "Compiling the fragment shader."
        f <- createShader FragmentShader
        shaderSourceBS f $= fragSrc
        compileShader f
        f'Ok <- get $ compileStatus f
        unless f'Ok $ do
            flog <- get $ shaderInfoLog f
            putStrLn $ "Log:" ++ flog
            exitFailure

        printError

        -- Both in a program!
        p <- createProgram
        putStrLn "Compiling the shader program."
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

        printError

        -- Create some geometry and store it in a VBO.
        putStrLn "Creating some geometry."
        let verts = [ 1.0, 0.0, 0.0
                    , 0.0, 1.0, 0.0
                    , 1.0, 1.0, 0.0
                    ] :: [Float]
            sizeiptr = length verts * sizeOf (undefined :: Float)
        vbo <- genObjectName
        bindBuffer ArrayBuffer $= Just vbo
        withArray verts $ \ptr ->
            bufferData ArrayBuffer $= (fromIntegral sizeiptr, ptr, StaticDraw)
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)

        printError

        putStrLn "Done initing resources."
        return g { _geometry = Just vbo }

    onInput i game =
        let keys  = _keysPressed $ _inputState i
            game' = game { _keys = keys }
        in case keys of
            Key'Escape:_ -> game' { _quit = True }
            _            -> game'

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

printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)

