{-# LANGUAGE OverloadedStrings #-}
module Graphics.QuadRenderer (
    initQuadRenderer
) where

import           Graphics.Utils
import           Graphics.Types
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw
import           Control.Monad
import           System.Exit            ( exitFailure )
import           Foreign.Storable       ( sizeOf )
import           Foreign.Marshal.Array  ( withArray )
import           Foreign.Ptr            ( nullPtr )
import qualified Data.ByteString as B


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


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


colorDescriptor :: VertexArrayDescriptor [Float]
colorDescriptor = VertexArrayDescriptor 4 Float 0 nullPtr




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


initQuadRenderer :: IO QuadRenderer
initQuadRenderer = do
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

    return QuadRenderer { _quadProgram = RndrProgram3D { _program = p
                                                       , _setModelview  = updateMV
                                                       , _setProjection = updatePJ
                                                       }
                        , _rndrQuad = rndrQuad
                        , _setQuadColor = updateColor
                        }

