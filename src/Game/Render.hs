module Game.Render where

import           Game.Types
import           App.Input
import           Graphics.Renderer
import           Math.Matrix
import           Data.Maybe
import           Control.Monad
import           Graphics.Rendering.OpenGL hiding ( renderer, Matrix )


renderGame :: Game -> IO Game 
renderGame game = do
    when (isJust $ _renderer game) $ do
        let renderer = fromJust $ _renderer game
            (w,h)  = _windowSize $ _inputState $ _input game
            pMat   = orthoMatrix 0 (fromIntegral w) 0 (fromIntegral h) 0 1 :: Matrix GLfloat
            l      = 20 :: GLfloat
            mat    = multiply (identityN 4 :: Matrix GLfloat) $ scaleMatrix3d l l 1
            colors = [ Color4 1.0 0.0 0.0 1.0
                     , Color4 0.0 0.0 1.0 1.0
                     , Color4 1.0 0.5 0.0 1.0
                     , Color4 1.0 1.0 0.0 1.0
                     , Color4 0.0 1.0 0.0 1.0
                     , Color4 0.5 0.0 0.5 1.0
                     , Color4 1.0 0.0 0.0 1.0
                     ]
            renders= [ _rndrI
                     , _rndrJ
                     , _rndrL
                     , _rndrO
                     , _rndrS
                     , _rndrT
                     , _rndrZ
                     ]
            (_,mats) = foldl trans (mat,[]) [0..6 :: Int]
            params = zip3 colors mats renders
            trans (m,ms) _  =
                let m' = translationMatrix3d 0 (l*2.0) 0 `multiply` m
                in (m',ms ++ [m])

        viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
        _updateProjection renderer $ concat pMat
        forM_ params $ \(c, m, r) -> do
            _updateColor renderer c
            _updateModelview renderer $ concat m
            r renderer

    return game
