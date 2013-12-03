{-# LANGUAGE OverloadedStrings #-}
module Graphics.Renderer where

import           Graphics.Utils
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw
import           Control.Monad
import           Data.List              ( intercalate )
import           System.Exit            ( exitFailure )
import           Foreign.Storable       ( sizeOf )
import           Foreign.Marshal.Array  ( withArray )
import           Foreign.Ptr            ( nullPtr )
import qualified Data.ByteString as B

data Renderer = Renderer { _screenSize  :: (GLfloat, GLfloat)
                         , _program     :: Program
                         , _rndrQuad    :: IO ()
                         , _updateColor :: Color4 GLfloat -> IO ()
                         , _updateProjection :: [GLfloat] -> IO ()
                         , _updateModelview  :: [GLfloat] -> IO ()
                         }

instance Show Renderer where
    show _ = "Renderer"


emptyRenderer :: Renderer
emptyRenderer = Renderer { _screenSize  = undefined
                         , _program     = undefined
                         , _rndrQuad    = undefined
                         , _updateColor = undefined
                         , _updateProjection = undefined
                         , _updateModelview  = undefined
                         }


type Rendering = IO (IO ())


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


colorDescriptor :: VertexArrayDescriptor [Float]
colorDescriptor = VertexArrayDescriptor 4 Float 0 nullPtr


makeShader :: ShaderType -> B.ByteString -> IO Shader
makeShader ty src = do
    putStrLn $ "Compiling a " ++ show ty
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


vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
    [ "attribute vec2 position;"

    , "uniform mat4 modelview;"
    , "uniform mat4 projection;"

    , "void main () {"
    , "  gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "uniform vec4 color;"

    , "void main() {"
    , "  gl_FragColor = color;"
    , "}"
    ]


makeProgram :: [Shader] -> [(String, AttribLocation)] -> IO Program
makeProgram shaders attributes = do
    p <- createProgram
    putStrLn "Compiling a shader program."
    mapM_ (attachShader p) shaders
    mapM_ (\(name, loc) -> attribLocation p name $= loc) attributes
    attribLocation p "position" $= AttribLocation 0
    attribLocation p "color" $= AttribLocation 1
    linkProgram p
    p'Ok <- get $ linkStatus p
    validateProgram p
    status <- get $ validateStatus p
    unless (p'Ok && status) $ do
        plog <- get $ programInfoLog p
        putStrLn plog
        printError
        exitFailure
    return p


bindVBO :: BufferObject -> VertexArrayDescriptor a -> AttribLocation -> IO ()
bindVBO vbo dsc loc = do
    bindBuffer ArrayBuffer $= Just vbo
    vertexAttribPointer loc $= (ToFloat, dsc)
    vertexAttribArray loc $= Enabled


drawArraysWith :: BufferObject -> PrimitiveMode -> Int -> IO ()
drawArraysWith vbo mode num = do
    bindVBO vbo vertDescriptor $ AttribLocation 0
    drawArrays mode 0 $ fromIntegral num
    bindBuffer ArrayBuffer $= Nothing


drawElementsWith :: BufferObject -> BufferObject -> PrimitiveMode -> Int -> IO ()
drawElementsWith i vbo mode num = do
    bindVBO vbo vertDescriptor $ AttribLocation 0
    bindBuffer ElementArrayBuffer $= Just i
    drawElements mode (fromIntegral num) UnsignedByte nullPtr
    bindBuffer ArrayBuffer $= Nothing
    bindBuffer ElementArrayBuffer $= Nothing


makeVBO :: [Float] -> IO BufferObject
makeVBO verts = do
    let size = length verts * sizeOf (undefined :: Float)
    vbo <- genObjectName
    bindVBO vbo vertDescriptor $ AttribLocation 0
    withArray verts $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
    return vbo


makeElementVBO :: [GLubyte] -> IO BufferObject
makeElementVBO indices = do
    let size = length indices * sizeOf (undefined :: GLubyte)
    i <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray indices $ \ptr ->
        bufferData ElementArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
    bindBuffer ElementArrayBuffer $= Nothing
    return i


makeRenderQuad :: Rendering
makeRenderQuad = do
    -- Create some geometry and store it in a VBO.
    let verts = [ 0.0, 0.0
                , 1.0, 0.0
                , 1.0, 1.0
                , 0.0, 1.0
                ] :: [Float]

    vbo <- makeVBO verts

    return $ drawArraysWith vbo TriangleFan 4


initRenderer :: IO Renderer
initRenderer = do
    -- Display some info about opengl
    vendorStr   <- get vendor
    rendererStr <- get renderer
    versionStr  <- get glVersion
    exts        <- get glExtensions
    glslV       <- get shadingLanguageVersion

    putStrLn $ intercalate "\n" [ "Vendor:" ++ vendorStr
                                , "Renderer:" ++ rendererStr
                                , "OpenGL Version:" ++ versionStr
                                , "GLSL Version:" ++ glslV
                                , "Extensions:\n  [ " ++ intercalate "\n  , " exts ++ "\n  ]"
                                ]

    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing 

    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    -- Both in a program!
    p <- makeProgram [v, f] [("position", AttribLocation 0), ("color", AttribLocation 1)]
    currentProgram $= Just p

    printError

    rndrQuad <- makeRenderQuad

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"
    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr

    cLoc <- get $ uniformLocation p "color"
    let updateColor c = uniform cLoc $= c

    return Renderer { _screenSize = (0,0)
                    , _program  = p
                    , _rndrQuad = rndrQuad
                    , _updateModelview  = updateMV
                    , _updateProjection = updatePJ
                    , _updateColor = updateColor
                    }


