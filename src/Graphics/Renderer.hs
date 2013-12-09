module Graphics.Renderer where

import           Graphics.Types
import           Graphics.QuadRenderer
import           Graphics.TextRenderer
import           Graphics.Rendering.OpenGL
import           Data.List (intercalate)


printGraphicStats :: IO ()
printGraphicStats = do
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

setGraphicDefaults :: IO ()
setGraphicDefaults = do
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    depthFunc $= Nothing


initRenderer :: FilePath -> IO Renderer
initRenderer fp = do
    printGraphicStats
    setGraphicDefaults
    quadRndr <- initQuadRenderer
    textRndr  <- initTextRenderer fp
    return $ Renderer { _screenSize = (0,0)
                      , _quadRndr   = quadRndr
                      , _textRndr   = textRndr
                      }

