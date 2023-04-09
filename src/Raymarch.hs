module Raymarch (runRaymarcher, Config(..)) where

import World.Shape
import Graphics.Gloss.Data.Color (Color, black)
import Control.Monad.State.Strict

data Config = Config
  { maxSteps :: Int
  , maxDistanceSq :: Double
  , epsilon :: Double
  , initialDirectionVector :: Point
  , initialPositionVector :: Point
  , fog :: Double -> Color
  , sun :: Point
  , shape :: Shape
  }

data RaymarcherState = RaymarcherState
  { directionVector :: Point
  , config :: Config
  , positionVector :: Point
  , steps :: Int
  }

type Raymarcher = State RaymarcherState

initialRaymarcherState :: Config -> RaymarcherState
initialRaymarcherState config = RaymarcherState
  { directionVector = normalise (initialDirectionVector config)
  , config = config
  , positionVector = initialPositionVector config
  , steps = 0
  }

distanceFromStartSq :: Raymarcher Double
distanceFromStartSq = do
  pos <- gets (initialPositionVector . config)
  pos' <- gets positionVector
  pure $ (pos' `sub` pos) `dot` (pos' `sub` pos)

hasEscaped :: Raymarcher Bool
hasEscaped = do
  raymarcherConfig <- gets config
  (maxDistanceSq raymarcherConfig <) <$> distanceFromStartSq

hasReachedMaxSteps :: Raymarcher Bool
hasReachedMaxSteps = do
  raymarcherConfig <- gets config
  (maxSteps raymarcherConfig <) <$> gets steps

hasCollided :: Raymarcher Bool
hasCollided = do
  raymarcherDistance <- gets (distance . shape . config) <*> gets positionVector
  raymarcherEpsilon <- gets (epsilon . config)
  pure (raymarcherDistance < raymarcherEpsilon)

step :: Raymarcher ()
step = do
  pos <- gets positionVector
  dir <- gets directionVector
  dist <- gets (distance . shape . config) <*> gets positionVector
  modify $ \s -> s { positionVector = pos `add` (dir `mul` dist), steps = steps s + 1 } 

getBaseColour :: Raymarcher Color
getBaseColour = gets (colour . shape . config) <*> gets positionVector

runRaymarcher :: Config -> Color
runRaymarcher = evalState raymarcher . initialRaymarcherState

raymarcher :: Raymarcher Color
raymarcher = do
  hasEscaped' <- hasEscaped
  hasReachedMaxSteps' <- hasReachedMaxSteps
  hasCollided' <- hasCollided
  if hasEscaped' || hasReachedMaxSteps'
    then pure black
  else if hasCollided'
    then getBaseColour
  else step >> raymarcher

------
-- point utility function
-- TODO: move to a separate module and use a newtype
------

-- | Vector addition on points.
add :: Point -> Point -> Point
add (x1, y1, z1) (x2, y2, z2) = (x1 + x2, y1 + y2, z1 + z2)

-- | Vector-scalar multiplication on points.
mul :: Point -> Double -> Point
mul (x, y, z) s = (x * s, y * s, z * s)

-- | Vector subtraction on points.
sub :: Point -> Point -> Point
sub (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

-- | Dot product on points.
dot :: Point -> Point -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1 * x2 + y1 * y2 + z1 * z2

mag :: Point -> Double
mag (x, y, z) = sqrt (x * x + y * y + z * z)

-- | Normalise a point.
normalise :: Point -> Point
normalise v = mul v (1 / mag v)