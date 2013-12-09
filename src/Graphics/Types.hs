{-# LANGUAGE TemplateHaskell #-}
module Graphics.Types where

import Graphics.Rendering.OpenGL
import Control.Lens


type Rendering = IO (IO ())


type MatrixUpdate = [GLfloat] -> IO ()


data RndrProgram3D = RndrProgram3D { _program :: Program
                                   , _updateProjection :: MatrixUpdate
                                   , _updateModelview  :: MatrixUpdate
                                   }
makeLenses ''RndrProgram3D


data TextRenderer = TextRenderer { _textProgram :: RndrProgram3D
                                 , _updateSampler :: Index1 GLint -> IO ()
                                 , _drawText :: String -> IO ()
                                 }
makeLenses ''TextRenderer

data QuadRenderer = QuadRenderer { _quadProgram :: RndrProgram3D
                                 , _rndrQuad    :: IO ()
                                 , _updateColor :: Color4 GLfloat -> IO ()
                                 }
makeLenses ''QuadRenderer

data Renderer = Renderer { _quadRndr   :: QuadRenderer
                         , _textRndr   :: TextRenderer
                         , _screenSize :: (GLfloat, GLfloat)
                         }
makeLenses ''Renderer

instance Show Renderer where
    show _ = "Renderer"

