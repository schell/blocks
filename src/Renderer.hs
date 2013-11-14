{-# LANGUAGE OverloadedStrings #-}
module Renderer where

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

data Renderer = Renderer { _program :: Program
                         , _rndrTri :: IO ()
                         , _rndrQuad:: IO ()
                         , _updateModelview  :: [GLfloat] -> IO ()
                         , _updateProjection :: [GLfloat] -> IO ()
                         }

instance Show Renderer where
    show _ = "Renderer"

type Rendering = IO (IO ())


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 3 Float 0 nullPtr


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
    [ "attribute vec3 position;"
    , "attribute vec4 color;"

    , "varying vec4 vColor;"

    , "uniform mat4 modelview;"
    , "uniform mat4 projection;"

    , "void main () {"
    , "  vColor = color;"
    , "  gl_Position = projection * modelview * vec4(position, 1);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec4 vColor;"

    , "void main() {"
    , "  gl_FragColor = vColor;"
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


makeVBOs :: [Float] -> [Float] -> IO (BufferObject, BufferObject)
makeVBOs verts tints = do
    let vSize = length verts * sizeOf (undefined :: Float)
        tSize = length tints * sizeOf (undefined :: Float)

    putStrLn "Making VBOs."
    [vvbo, cvbo] <- genObjectNames 2

    -- Bind and buffer our vertex data.
    bindVBO vvbo vertDescriptor $ AttribLocation 0
    withArray verts $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral vSize, ptr, StaticDraw)

    -- Bind and buffer our color data.
    bindVBO cvbo colorDescriptor $ AttribLocation 1
    withArray tints $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral tSize, ptr, StaticDraw)
    printError
    return (vvbo, cvbo)


makeTriRender :: Rendering
makeTriRender = do
    -- Create some geometry and store it in a VBO.
    putStrLn "Creating some geometry."
    let verts = [ 0.0, 1.0, 0.0
                , 1.0, 1.0, 0.0
                , 0.5, 0.0, 0.0
                ] :: [Float]
        tints = [ 1.0, 1.0, 0.0, 1.0
                , 0.0, 1.0, 1.0, 1.0
                , 1.0, 0.0, 1.0, 1.0
                ] :: [Float]

    (vvbo, cvbo) <- makeVBOs verts tints

    putStrLn "Done initing resources."
    return $ do bindVBO vvbo vertDescriptor $ AttribLocation 0
                bindVBO cvbo colorDescriptor $ AttribLocation 1
                drawArrays Triangles 0 3
                bindBuffer ArrayBuffer $= Nothing


makeQuadRender :: Rendering
makeQuadRender = do
    -- Create some geometry and store it in a VBO.
    putStrLn "Creating some geometry."
    let verts = [ 0.0, 0.0, 0.0
                , 1.0, 0.0, 0.0
                , 1.0, 1.0, 0.0
                , 0.0, 1.0, 0.0
                ] :: [Float]
        tints = [ 1.0, 1.0, 0.0, 1.0
                , 0.0, 1.0, 1.0, 1.0
                , 1.0, 0.0, 1.0, 1.0
                , 1.0, 1.0, 1.0, 1.0
                ] :: [Float]

    (vvbo, cvbo) <- makeVBOs verts tints

    putStrLn "Done initing resources."
    return $ do bindVBO vvbo vertDescriptor $ AttribLocation 0
                bindVBO cvbo colorDescriptor $ AttribLocation 1
                drawArrays TriangleFan 0 4
                bindBuffer ArrayBuffer $= Nothing


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

    quadIO <- makeQuadRender
    triIO  <- makeTriRender

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj  <- get $ uniformLocation p "projection"
    let updateMV = (\mat ->
                       withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr)
        updatePJ = (\mat ->
                       withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr)

    return Renderer { _program  = p
                    , _rndrTri  = triIO
                    , _rndrQuad = quadIO
                    , _updateModelview  = updateMV
                    , _updateProjection = updatePJ
                    }


