{-# LANGUAGE OverloadedStrings #-}
module Main where

import           App.App
import           App.TypeClasses
import           App.Input
import           Graphics.Rendering.OpenGL
import           Graphics.UI.GLFW
import           Data.Maybe
import           Control.Monad
import           Data.List              ( intercalate )
import           System.Exit            ( exitFailure )
import           Foreign.Storable       ( sizeOf )
import           Foreign.Marshal.Array  ( withArray )
import           Foreign.Ptr            ( nullPtr )
import           System.IO              ( hPutStrLn, stderr )
import qualified Data.ByteString as B

-- | The root of our game data.
data Game = Game { _quit     :: Bool -- ^ Whether or not the game should quit.
                 , _geometry :: Maybe BufferObject -- ^ A graphic resource.
                 , _keys     :: [Key] -- ^ Current key state.
                 } deriving (Show)


-- | Creates a default game.
newGame :: Game
newGame = Game { _quit = False
               , _geometry = Nothing
               , _keys = []
               }

vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 3 Float 0 nullPtr


main :: IO ()
main = void $ initializeApp newGame >>= startApp


instance UserData Game where
    onStart g = do
        -- Display some info about opengl
        vendorStr <- get vendor
        rendererStr <- get renderer
        versionStr <- get glVersion
        exts <- get glExtensions
        glslV <- get shadingLanguageVersion
        putStrLn $ intercalate "\n" [ "Vendor:" ++ vendorStr
                                    , "Renderer:" ++ rendererStr
                                    , "OpenGL Version:" ++ versionStr
                                    , "GLSL Version:" ++ glslV
                                    , "Extensions:\n  [ " ++ intercalate "\n  , " exts ++ "\n  ]"
                                    ]

        v <- makeShader VertexShader vertSrc
        f <- makeShader FragmentShader fragSrc

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
            exitFailure
        currentProgram $= Just p

        printError

        -- Create some geometry and store it in a VBO.
        putStrLn "Creating some geometry."
        let verts = [ -1.0, -1.0, 0.0
                    ,  1.0, -1.0, 0.0
                    ,  0.0,  1.0, 0.0
                    ] :: [Float]
            sizeiptr = length verts * sizeOf (undefined :: Float)

        putStrLn "Making a VAO."
        vao <- genObjectName
        bindVertexArrayObject $= Just vao
        printError

        putStrLn "Making a VBO."
        vbo <- genObjectName
        bindBuffer ArrayBuffer $= Just vbo
        withArray verts $ \ptr ->
            bufferData ArrayBuffer $= (fromIntegral sizeiptr, ptr, StaticDraw)
        vertexAttribPointer (AttribLocation 0) $= (ToFloat, vertDescriptor)
        vertexAttribArray (AttribLocation 0) $= Enabled
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

    onRender game = do when (isJust $ _geometry game) $ do
                           bindBuffer ArrayBuffer $= _geometry game
                           vertexAttribPointer (AttribLocation 0) $= (ToFloat, vertDescriptor)
                           drawArrays Triangles 0 3
                           printError
                       return game

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


makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    putStrLn "Compiling the vertex shader."
    s <- createShader ty
    shaderSourceBS s $= src
    compileShader s
    s'Ok <- get $ compileStatus s
    unless s'Ok $ do
        slog <- get $ shaderInfoLog s
        putStrLn $ "Log:" ++ slog
        exitFailure
    printError
    return s

