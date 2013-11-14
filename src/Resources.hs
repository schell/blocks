{-# LANGUAGE OverloadedStrings #-}
module Resources where

import           Utils
import           Graphics.Rendering.OpenGL
import           Control.Monad
import           Data.List              ( intercalate )
import           System.Exit            ( exitFailure )
import           Foreign.Storable       ( sizeOf )
import           Foreign.Marshal.Array  ( withArray )
import           Foreign.Ptr            ( nullPtr )
import qualified Data.ByteString as B


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

    , "void main () {"
    , "  vColor = color;"
    , "  gl_Position = vec4(position, 1);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec4 vColor;"

    , "void main() {"
    , "  gl_FragColor = vColor;"
    , "}"
    ]


initResources :: IO VertexArrayObject
initResources = do
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
    p <- createProgram
    putStrLn "Compiling the shader program."
    attachShader p v
    attachShader p f
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
    currentProgram $= Just p

    printError

    -- Create some geometry and store it in a VBO.
    putStrLn "Creating some geometry."
    let verts = [ -1.0, -1.0, 0.0
                ,  1.0, -1.0, 0.0
                ,  0.0,  1.0, 0.0
                ] :: [Float]
        vSize = length verts * sizeOf (undefined :: Float)
        tints = [ 1.0, 1.0, 0.0, 1.0
                , 0.0, 1.0, 1.0, 1.0
                , 1.0, 0.0, 1.0, 1.0
                ] :: [Float]
        tSize = length verts * sizeOf (undefined :: Float)

    putStrLn "Making a VAO."
    vao <- genObjectName
    bindVertexArrayObject $= Just vao
    printError

    putStrLn "Making VBOs."
    [vvbo, cvbo] <- genObjectNames 2
    -- Buffer our vertex data.
    bindBuffer ArrayBuffer $= Just vvbo
    withArray verts $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral vSize, ptr, StaticDraw)
    vertexAttribPointer (AttribLocation 0) $= (ToFloat, vertDescriptor)
    vertexAttribArray (AttribLocation 0) $= Enabled

    -- Buffer our color data.
    bindBuffer ArrayBuffer $= Just cvbo
    withArray tints $ \ptr ->
        bufferData ArrayBuffer $= (fromIntegral tSize, ptr, StaticDraw)
    vertexAttribPointer (AttribLocation 1) $= (ToFloat, colorDescriptor)
    vertexAttribArray (AttribLocation 1) $= Enabled

    printError

    putStrLn "Done initing resources."
    return vao
