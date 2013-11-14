module Utils where

import           Graphics.Rendering.OpenGL
import           System.IO  ( hPutStrLn, stderr )


printError :: IO ()
printError = get errors >>= mapM_ (hPutStrLn stderr . ("GL: "++) . show)


