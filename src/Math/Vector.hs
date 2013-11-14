module Math.Vector where

import Prelude hiding ( subtract )

-- Vector Types

type Vector a = [a]

type Vec2 a = (a, a)
type Vec3 a = (a, a, a)
type Vec4 a = (a, a, a, a)

-- Vector functions

-- | Computes the magnitude.
magnitude :: Floating a => Vector a -> a
magnitude = sqrt . sum . map (**2)

-- | Computes the unit vector.
normalize :: Floating a => [a] -> [a]
normalize vec = map (/mag) vec
    where mag = magnitude vec

-- | Computes the unit vector.
unitize :: Floating a => [a] -> [a]
unitize = normalize

-- | Adds two vectors.
add :: Floating a => [a] -> [a] -> [a]
add = zipWith (+)

-- | Subtracts two vectors.

subtract :: Num a => [a] -> [a] -> [a]
subtract = zipWith (-)
