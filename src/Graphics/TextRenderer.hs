{-# LANGUAGE OverloadedStrings #-}
module Graphics.TextRenderer where

import           Graphics.Utils
import           Graphics.Types
import           Graphics.LoadTexture
import           Graphics.Rendering.OpenGL
import           Graphics.Rendering.OpenGL.Raw
import           Codec.Picture
import           Control.Monad
import           Data.Maybe
import           System.Exit            (exitFailure)
import           Data.Vector.Storable   (unsafeWith)
import           Foreign.Marshal.Array  (withArray)
import           Foreign.Storable       ( sizeOf )
import           Foreign.Ptr            (nullPtr)
import qualified Data.ByteString as B


vertSrc :: B.ByteString
vertSrc = B.intercalate "\n"
    [ "attribute vec2 position;"
    , "attribute vec2 uv;"

    , "varying vec2 vTex; "

    , "uniform mat4 modelview;"
    , "uniform mat4 projection;"

    , "void main () {"
    , "    vTex = uv;"
    , "    gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]


fragSrc :: B.ByteString
fragSrc = B.intercalate "\n"
    [ "varying vec2 vTex;"

    , "uniform sampler2D sampler;"

    , "void main() {"
    , "    gl_FragColor = texture2D(sampler, vec2(vTex.s,vTex.t));"
    , "}"
    ]


vertDescriptor :: VertexArrayDescriptor [Float]
vertDescriptor = VertexArrayDescriptor 2 Float 0 nullPtr


uvDescriptor :: VertexArrayDescriptor [Float]
uvDescriptor = vertDescriptor


initTextRenderer :: FilePath -> IO TextRenderer
initTextRenderer texFP = do
    putStrLn "Text shader."
    v <- makeShader VertexShader vertSrc
    f <- makeShader FragmentShader fragSrc

    p <- makeProgram [v,f] [("position", AttribLocation 0), ("uv", AttribLocation 1)]
    currentProgram $= Just p
    printError

    UniformLocation mv <- get $ uniformLocation p "modelview"
    UniformLocation pj <- get $ uniformLocation p "projection"

    let updateMV mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv mv 1 1 ptr
        updatePJ mat = withArray mat $ \ptr ->
                           glUniformMatrix4fv pj 1 1 ptr

    sLoc <- get $ uniformLocation p "sampler"
    let updateSampler s = uniform sLoc $= s

    -- Load the texture.
    mTObj <- initTexture (texFP ++ "/text.png") 0
    when (isNothing mTObj) $ do
        putStrLn "Could not load texture."
        exitFailure

    let Just t = mTObj

    let drawText s = do let verts  = stringToVerts s
                            uvs    = stringToUVs s
                            size   = length verts * sizeOf (undefined :: Float)
                        [i,j] <- genObjectNames 2
                        -- Buffer the verts
                        bindVBO i vertDescriptor $ AttribLocation 0
                        withArray verts $ \ptr ->
                            bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)
                        -- Buffer the uvs
                        bindVBO j uvDescriptor $ AttribLocation 1
                        withArray uvs $ \ptr ->
                            bufferData ArrayBuffer $= (fromIntegral size, ptr, StaticDraw)

                        texture Texture2D $= Enabled
                        activeTexture $= TextureUnit 0
                        textureBinding Texture2D $= Just t
                        bindVBO i vertDescriptor $ AttribLocation 0
                        bindVBO j uvDescriptor $ AttribLocation 1
                        drawArrays Triangles 0 $ fromIntegral (6*length s)
                        bindBuffer ArrayBuffer $= Nothing
                        deleteObjectNames [i,j]

    return TextRenderer { _textProgram    = RndrProgram3D { _program = p
                                                          , _updateModelview  = updateMV
                                                          , _updateProjection = updatePJ
                                                          }
                        , _updateSampler = updateSampler
                        , _drawText = drawText
                        }

