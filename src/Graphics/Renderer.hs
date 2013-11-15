{-# LANGUAGE OverloadedStrings #-}
module Graphics.Renderer where

import           Utils
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw
import           Control.Monad
import           Data.List              ( intercalate )
import           System.Exit            ( exitFailure )
import           Foreign.Storable       ( sizeOf )
import           Foreign.Marshal.Array  ( withArray )
import           Foreign.Ptr            ( nullPtr )
import qualified Data.ByteString as B

data Renderer = Renderer { _program  :: Program
                         , _rndrTri  :: IO ()
                         , _rndrQuad :: IO ()
                         , _rndrI    :: IO ()
                         , _rndrJ    :: IO ()
                         , _rndrL    :: IO ()
                         , _rndrO    :: IO ()
                         , _rndrS    :: IO ()
                         , _rndrT    :: IO ()
                         , _rndrZ    :: IO ()
                         , _updateModelview  :: [GLfloat] -> IO ()
                         , _updateProjection :: [GLfloat] -> IO ()
                         , _updateColor :: Color4 GLfloat -> IO ()
                         }

instance Show Renderer where
    show _ = "Renderer"

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


makeVBO :: [Float] -> IO BufferObject
makeVBO verts = do
    let size = length verts * sizeOf (undefined :: Float)

    vbo <- genObjectName

    -- Bind and buffer our vertex data.
    bindVBO vbo vertDescriptor $ AttribLocation 0
    withArray verts $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)

    printError
    return vbo


makeRenderTri :: Rendering
makeRenderTri = do
    -- Create some geometry and store it in a VBO.
    let verts = [ 0.0, 1.0
                , 1.0, 1.0
                , 0.5, 0.0
                ] :: [Float]

    vbo <- makeVBO verts

    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays Triangles 0 3
                bindBuffer ArrayBuffer $= Nothing


makeRenderQuad :: Rendering
makeRenderQuad = do
    -- Create some geometry and store it in a VBO.
    let verts = [ 0.0, 0.0
                , 1.0, 0.0
                , 1.0, 1.0
                , 0.0, 1.0
                ] :: [Float]

    vbo <- makeVBO verts

    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays TriangleFan 0 4
                bindBuffer ArrayBuffer $= Nothing


makeRenderI :: Rendering
makeRenderI = do
    let verts = [ 0.0, 0.0
                , 4.0, 0.0
                , 4.0, 1.0
                , 0.0, 1.0
                ]
    vbo <- makeVBO verts
    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays TriangleFan 0 4
                bindBuffer ArrayBuffer $= Nothing


makeRenderJ :: Rendering
makeRenderJ = do
    let verts = [ 3.0, 0.0
                , 0.0, 0.0
                , 0.0, 1.0
                , 2.0, 1.0
                , 2.0, 2.0
                , 3.0, 2.0
                ]
    vbo <- makeVBO verts
    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays TriangleFan 0 6
                bindBuffer ArrayBuffer $= Nothing


makeRenderL :: Rendering
makeRenderL = do
    let verts = [ 0.0, 0.0
                , 3.0, 0.0
                , 3.0, 1.0
                , 1.0, 1.0
                , 1.0, 2.0
                , 0.0, 2.0
                ]
    vbo <- makeVBO verts
    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays TriangleFan 0 6
                bindBuffer ArrayBuffer $= Nothing


makeRenderO :: Rendering
makeRenderO = do
    let verts = [ 0.0, 0.0
                , 2.0, 0.0
                , 2.0, 2.0
                , 0.0, 2.0
                ]
    vbo <- makeVBO verts
    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays TriangleFan 0 4
                bindBuffer ArrayBuffer $= Nothing


makeRenderS :: Rendering
makeRenderS = do
    let verts   = [ 1.0, 0.0
                  , 2.0, 0.0
                  , 3.0, 0.0

                  , 0.0, 1.0
                  , 1.0, 1.0
                  , 2.0, 1.0
                  , 3.0, 1.0

                  , 0.0, 2.0
                  , 1.0, 2.0
                  , 2.0, 2.0
                  ]
        indices = [ 0, 1, 4
                  , 1, 4, 5
                  , 1, 2, 5
                  , 2, 5, 6
                  , 3, 4, 7
                  , 4, 7, 8
                  , 4, 5, 8
                  , 5, 8, 9
                  ] :: [GLubyte]
        size    = (length indices) * sizeOf (undefined :: GLubyte)
    vbo <- makeVBO verts
    i   <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray indices $ \ptr ->
        bufferData ElementArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
    bindBuffer ElementArrayBuffer $= Nothing

    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                bindBuffer ElementArrayBuffer $= Just i
                drawElements Triangles 24 UnsignedByte nullPtr
                bindBuffer ArrayBuffer $= Nothing
                bindBuffer ElementArrayBuffer $= Nothing


makeRenderT :: Rendering
makeRenderT = do
    let verts = [ 1.5, 0.0
                , 3.0, 0.0
                , 3.0, 1.0
                , 2.0, 1.0
                , 2.0, 2.0
                , 1.0, 2.0
                , 1.0, 1.0
                , 0.0, 1.0
                , 0.0, 0.0
                ]
    vbo <- makeVBO verts
    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                drawArrays TriangleFan 0 9
                bindBuffer ArrayBuffer $= Nothing


makeRenderZ :: Rendering
makeRenderZ = do
    let verts   = [ 0.0, 0.0 
                  , 1.0, 0.0
                  , 2.0, 0.0

                  , 0.0, 1.0
                  , 1.0, 1.0
                  , 2.0, 1.0
                  , 3.0, 1.0

                  , 1.0, 2.0
                  , 2.0, 2.0
                  , 3.0, 2.0
                  ]
        indices = [ 0, 1, 3
                  , 1, 3, 4
                  , 1, 2, 4
                  , 2, 4, 5
                  , 4, 5, 7
                  , 5, 7, 8
                  , 5, 6, 8
                  , 6, 8, 9
                  ] :: [GLubyte]
        size    = (length indices) * sizeOf (undefined :: GLubyte)
    vbo <- makeVBO verts
    i   <- genObjectName
    bindBuffer ElementArrayBuffer $= Just i
    withArray indices $ \ptr ->
        bufferData ElementArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
    bindBuffer ElementArrayBuffer $= Nothing

    return $ do bindVBO vbo vertDescriptor $ AttribLocation 0
                bindBuffer ElementArrayBuffer $= Just i
                drawElements Triangles 24 UnsignedByte nullPtr
                bindBuffer ArrayBuffer $= Nothing
                bindBuffer ElementArrayBuffer $= Nothing




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

    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    -- Both in a program!
    p <- makeProgram [v, f] [("position", AttribLocation 0), ("color", AttribLocation 1)]
    currentProgram $= Just p

    printError

    rndrQuad <- makeRenderQuad
    rndrTri  <- makeRenderTri
    rndrI    <- makeRenderI
    rndrJ    <- makeRenderJ
    rndrL    <- makeRenderL
    rndrO    <- makeRenderO
    rndrS    <- makeRenderS
    rndrT    <- makeRenderT
    rndrZ    <- makeRenderZ

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"
    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr

    cLoc <- get $ uniformLocation p "color"
    let updateColor c = uniform cLoc $= c

    return Renderer { _program  = p
                    , _rndrTri  = rndrTri
                    , _rndrQuad = rndrQuad
                    , _rndrI    = rndrI
                    , _rndrJ    = rndrJ
                    , _rndrL    = rndrL
                    , _rndrO    = rndrO
                    , _rndrS    = rndrS
                    , _rndrT    = rndrT
                    , _rndrZ    = rndrZ
                    , _updateModelview  = updateMV
                    , _updateProjection = updatePJ
                    , _updateColor = updateColor
                    }


