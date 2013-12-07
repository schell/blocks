module Game.Tetris where

import Game.Types
import Game.Block
import Game.Board
import Graphics.Rendering.OpenGL
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Lens
import Debug.Trace


newTetris :: Tetris
newTetris = Tetris { _board = []
                   , _block = Nothing
                   , _timer = 0
                   , _gameOver = False
                   }


fallRate :: Double
fallRate = 1.0


updateTetris :: Double -> State Tetris ()
updateTetris dt = do
    hasQuit <- use gameOver
    when (not hasQuit) $ do
        -- Increase our time accumulator by dt.
        timer += dt
        -- Get the number of ticks we're supposed to perform (1).
        ticks <- uses timer $ floor . (/ fallRate)
        -- Set the time to the leftover after we take our ticks.
        timer -= fromIntegral ticks * fallRate
        -- Iterate the tetris rulse over the board `tick` number of times.
        replicateM_ ticks iterateTetrisRules


iterateTetrisRules :: State Tetris ()
iterateTetrisRules = do
    mBlk <- use block
    when (isJust mBlk) $ do
        -- Get the block as it is moved down 20px.
        blk <- uses block ((blockPos._2 +~ 20) . fromJust)
        -- Check if that block has hit the board and where.
        (hit, pos) <- uses board (`blockHasHit` blk)
        let blk' = blockPos .~ pos $ blk
        when hit $ do
            -- Set tetris' block to nil.
            block .= Nothing
            -- Stick the block at the right position on the board.
            board %= (blk':)
            -- Set the game over condition.
            gameOver .= (pos^._2 <= 0)
        when (not hit) $ do
            -- Update the block.
            block .= Just blk'
        -- Get the lines we've scored.
        foundLns <- uses board findLines
        board %= (`removeLines` foundLns)


stepTetris :: Tetris -> Double -> Tetris
stepTetris tetris dt
    | _gameOver tetris = tetris
    | otherwise = execState (updateTetris dt) tetris


blockHasHit :: Board -- ^ The board.
            -> Block -- ^ The block.
            -> (Bool, Pos) -- ^ The stop status and new position.
                           --   If status is True the block should be
                           --   added to the board.
blockHasHit b bl =
    let (x,y) = _blockPos bl
        p     = _blockPieces bl
        bh    = heightOfPieces p
        bw    = widthOfPieces p
        (w,h) = boardSize
    -- Check for going past the y boundary.
    in if y + bh > h
         then (True, (x, h - bh))
         -- Check for going past the x boundary.
         else if x < 0
                then (False, (0,y))
                else if x + bw > w
                       then (False, (w - bw,y))
                       else collideWithBoard b bl


collideWithBoard :: Board -> Block -> (Bool, Pos)
collideWithBoard b bl =
    let tests      = map (collideBlocks bl) b
        collisions = dropWhile (not . fst) tests
    in case collisions of
           hit:_ -> hit
           _     -> (False, _blockPos bl)


collideBlocks :: Block -> Block -> (Bool, Pos)
collideBlocks b1@(Block _ (x,y) _) b2 =
    let b2aabb = blockToAABB b2
    in case hitTestAABBs (blockToAABB b1) b2aabb of
           (False,_) -> (False, (x,y))

                            -- Get a list of the pieces as aabbs.
           (True,_)  -> let b1ps  = blockPiecesToAABBs b1
                            b2ps  = blockPiecesToAABBs b2
                            -- Collect the piece AABBs that intersect.
                            tests = concatMap (\b1p ->
                                        map (hitTestAABBs b1p) b2ps) b1ps
                            colls = filter fst tests
                            in case colls of
                               (True,(dx,dy)):_ -> (True, (x-dx,y-dy))
                               _                -> (False, (x,y))


type AABB = (GLfloat,GLfloat,GLfloat,GLfloat)


blockToAABB :: Block -> AABB
blockToAABB (Block _ (x,y) p) = (x,y,widthOfPieces p,heightOfPieces p)


blockPiecesToAABBs :: Block -> [AABB]
blockPiecesToAABBs (Block _ (x,y) p) = aabbs
    where yl = fromIntegral $ length p
          xl = fromIntegral $ length $ head p
          ys = map (*blockWidth) [0..yl] :: [GLfloat]
          xs = map (*blockWidth) [0..xl] :: [GLfloat]
          w  = blockWidth

          zipw b c a = zipWith a b c

          mAABBs = zipw ys p (\yinc row ->
                       zipw xs row (\xinc exists ->
                           if exists then Just (x + xinc, y + yinc,w,w) else Nothing))

          aabbs  = catMaybes $ concat mAABBs


-- | Tests the two AABBs for intersection. Returns a tuple of
-- whether or not they collided and a displacement vector.
hitTestAABBs :: AABB -> AABB -> (Bool, (GLfloat, GLfloat))
hitTestAABBs ab1 ab2 =
    case intersectAABBs ab1 ab2 of
        (0,0) -> (False, (0,0))
        proj  -> (True, proj)


intersectAABBs :: AABB -> AABB -> (GLfloat, GLfloat)
intersectAABBs b1@(x1, y1, w1, h1) b2@(x2, y2, w2, h2)
     | x1 >= x2 + w2 || x2 >= x1 + w1 = (0, 0)
     | y1 >= y2 + h2 || y2 >= y1 + h1 = (0, 0)
     | otherwise = projectAABBs b1 b2


projectAABBs :: AABB -> AABB -> (GLfloat, GLfloat)
projectAABBs (x1,y1,w1,h1) (x2,y2,w2,h2) =
    let x = calculateOverlap (x1,w1) (x2,w2)
        y = calculateOverlap (y1,h1) (y2,h2)
    in if y <= x then (0,y) else (x,0)


calculateOverlap :: (GLfloat, GLfloat) -> (GLfloat, GLfloat) -> GLfloat
calculateOverlap (c1,e1) (c2,e2) =
    let c1' = c1 + e1'
        e1' = e1/2
        c2' = c2 + e2'
        e2' = e2/2
    in (e1' + e2') - abs (c1' - c2')


moveBlockLeft :: Tetris -> Tetris
moveBlockLeft t = moveBlockHorizontalBy t (-blockWidth)


moveBlockRight :: Tetris -> Tetris
moveBlockRight t = moveBlockHorizontalBy t blockWidth


moveBlockDown :: Tetris -> Tetris
moveBlockDown t
    | t^.block == Nothing = t
    | otherwise = moveBlockDown $ execState iterateTetrisRules t


moveBlockHorizontalBy :: Tetris -> GLfloat -> Tetris
moveBlockHorizontalBy t@(Tetris _ mB _ _) xx = t { _block = mB' }
    where mB' = case mB of
                    Nothing -> Nothing
                    Just b  -> let (x,y) = _blockPos b
                                   b'    = b { _blockPos = (x+xx,y)}
                               in case blockHasHit (_board t) b' of
                                      (False,pos) -> Just (b & blockPos .~ pos)
                                      (True,_)    -> mB


rotateBlock :: Tetris -> Tetris
rotateBlock t@(Tetris _ mB _ _) = t { _block = mB' }
    where mB' = case mB of
                    Nothing -> Nothing
                    Just b  -> let b' = b & blockPieces %~ transpose . reverse
                               in case (t^.board) `blockHasHit` b' of
                                      (False, pos) -> Just (b' & blockPos .~ pos)
                                      (True,_)     -> mB


leftBoundaryOfPieces :: Pieces -> GLfloat
leftBoundaryOfPieces _ = 0


rightBoundaryOfPieces :: Pieces -> GLfloat
rightBoundaryOfPieces = (fst boardSize -) . widthOfPieces


widthOfPieces :: Pieces -> GLfloat
widthOfPieces = heightOfPieces . transpose . reverse


heightOfPieces :: Pieces -> GLfloat
heightOfPieces =
    (blockWidth*) . fromIntegral . length . filter (== True) . map or


