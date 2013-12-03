module Game.Board where

import Game.Types
import Game.Block
import Data.List


-- | Turns a board into a grid of possibly occupied values.
boardToGrid :: Board -> [[Bool]]
boardToGrid = foldl insertBlock emptyGrid


-- | Turns a block into a grid of possibly occupied values.
blockToGrid :: Block -> [[Bool]]
blockToGrid (Block _ (x,y) ps) =
    let x'    = floor $ x / blockWidth :: Int
        y'    = floor $ y / blockWidth :: Int
        w     = length $ head ps
        top   = take y' emptyGrid
        left  = replicate x' False
        mid   = map (\row -> left ++ row ++ right) ps
        right = replicate (10-x'- w) False
        bottom= take (20 - length ps - y') emptyGrid
    in top ++ mid ++ bottom


gridToBlockOfType :: [[Bool]] -> BlockType -> Block
gridToBlockOfType g t =
    let yproj = map or g
        y     = length $ takeWhile (==False) yproj
        xproj = map or $ transpose g
        x     = length $ takeWhile (==False) xproj
        w     = length $ takeWhile (==True) $ dropWhile (==False) xproj
        g'    = map (take w . drop x) g
        ps    = takeWhile or $ dropWhile (not . or) g'
    in Block t (fromIntegral x*blockWidth,fromIntegral y*blockWidth) ps


insertBlock :: [[Bool]] -> Block -> [[Bool]]
insertBlock g b = zipWith (zipWith (||)) g $ blockToGrid b


emptyGrid :: [[Bool]]
emptyGrid = replicate 20 $ replicate 10 False

findLines :: Board -> [Int]
findLines = elemIndices True . map and . boardToGrid


removeLines :: Board -> [Int] -> Board
removeLines b [] = b
removeLines b ls = map (removeLinesFromBlock ls) b


removeLinesFromBlock :: [Int] -> Block -> Block
removeLinesFromBlock [] b = b
removeLinesFromBlock l@(n:_) b@(Block t _ _) =
    let g   = blockToGrid b
        top = take (length l) emptyGrid
        rest= take n g ++ drop (1 + last l) g
        g'  = top ++ rest
    in gridToBlockOfType g' t

