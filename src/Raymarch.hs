module Raymarch (runRaymarcher, Config(..)) where

import World.Shape
import Control.Monad.State.Strict
import qualified Linear as L
import Graphics.Gloss.Raster.Field (Color, black, mixColors)

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
  { directionVector = initialDirectionVector config
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
  if hasEscaped'
    then pure black
  else if hasCollided' || hasReachedMaxSteps'
    then do
      v1 <- gets (sun . config)
      v2 <- gets (normal . epsilon . config) <*> gets (shape . config) <*> gets positionVector
      let lightingNormal = (v1 `L.dot` v2) / (L.norm v1 * L.norm v2)
      lit <- getIsLit
      let factor = if lightingNormal < 0 || not lit
            then 0.1
            else realToFrac $ 0.9*lightingNormal + 0.1
      mixColors (1 - factor) factor black <$> getBaseColour
  else step >> raymarcher

getIsLit :: Raymarcher Bool
getIsLit = evalState go . f <$> get
  where
    f s = s { directionVector = sun (config s), steps = 0 }
    go = do
      escapeSurface
      step
      hasEscaped' <- hasEscaped
      hasReachedMaxSteps' <- hasReachedMaxSteps
      hasCollided' <- hasCollided
      if hasEscaped' || hasReachedMaxSteps'
        then pure True
      else if hasCollided'
        then pure False
      else step >> go

escapeSurface :: Raymarcher ()
escapeSurface = do
  pos <- gets positionVector
  eps <- gets (epsilon . config)
  dir <- (\s -> normal eps s pos) <$> gets (shape . config)
  dist <- gets (epsilon . config)
  modify $ \s -> s { positionVector = pos + (dir L.^* dist) }