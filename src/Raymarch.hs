module Raymarch (runRaymarcher, Config(..)) where

import World.Shape
import Graphics.Gloss.Data.Color (Color, black)
import Control.Monad.State.Strict
import qualified Linear as L

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
  { directionVector = L.normalize (initialDirectionVector config)
  , config = config
  , positionVector = initialPositionVector config
  , steps = 0
  }

distanceFromStartSq :: Raymarcher Double
distanceFromStartSq = do
  pos <- gets (initialPositionVector . config)
  pos' <- gets positionVector
  pure $ L.quadrance (pos' - pos)

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
  modify $ \s -> s { positionVector = pos + (dir L.^* dist), steps = steps s + 1 } 

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
