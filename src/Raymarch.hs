{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Raymarch (runRaymarcher, Config(..)) where

import Prelude hiding ((||), (<), (>), (&&), not)
import World.Shape
import Control.Monad.State.Strict
import qualified Data.Array.Accelerate.Linear as AL
import Data.Array.Accelerate.Linear ()
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (ifThenElse, (||), (<), (>), not)
import Data.Array.Accelerate.Data.Colour.RGBA (Colour, blend)
import Data.Array.Accelerate.Data.Colour.Names (black)

data Config = Config
  { maxSteps :: A.Exp Int
  , maxDistanceSq :: A.Exp Double
  , epsilon :: A.Exp Double
  , initialDirectionVector :: A.Exp Point
  , initialPositionVector :: A.Exp Point
  , fog :: A.Exp Colour
  , sun :: A.Exp Point
  , shape :: Shape
  , ambientLighting :: A.Exp Double
  }

data RaymarcherState = RaymarcherState
  { directionVector :: A.Exp Point
  , config :: Config -- ^ NOTE: immutable - we assume config does not get modified at any point
  , positionVector :: A.Exp Point
  , steps :: A.Exp Int
  }

type Raymarcher = State RaymarcherState

initialRaymarcherState :: Config -> RaymarcherState
initialRaymarcherState config = RaymarcherState
  { directionVector = initialDirectionVector config
  , config = config
  , positionVector = initialPositionVector config
  , steps = 0
  }

distanceFromStartSq :: Raymarcher (A.Exp Double)
distanceFromStartSq = do
  pos <- gets (initialPositionVector . config)
  pos' <- gets positionVector
  pure $ AL.quadrance (pos' - pos)

hasEscaped :: Raymarcher (A.Exp Bool)
hasEscaped = do
  raymarcherConfig <- gets config
  (maxDistanceSq raymarcherConfig <) <$> distanceFromStartSq

hasReachedMaxSteps :: Raymarcher (A.Exp Bool)
hasReachedMaxSteps = do
  raymarcherConfig <- gets config
  (maxSteps raymarcherConfig <) <$> gets steps

hasCollided :: Raymarcher (A.Exp Bool)
hasCollided = do
  raymarcherDistance <- gets (distance . shape . config) <*> gets positionVector
  raymarcherEpsilon <- gets (epsilon . config)
  pure (raymarcherDistance < raymarcherEpsilon)

step :: Raymarcher ()
step = do
  pos <- gets positionVector
  dir <- gets directionVector
  dist <- gets (distance . shape . config) <*> gets positionVector
  modify $ \s -> s { positionVector = pos + (dir AL.^* dist), steps = steps s + 1 }

getBaseColour :: Raymarcher (A.Exp Colour)
getBaseColour = gets (colour . shape . config) <*> gets positionVector

runRaymarcher :: Config -> A.Exp Colour
runRaymarcher = evalState raymarcher . initialRaymarcherState

raymarcher :: Raymarcher (A.Exp Colour)
raymarcher = do
  hasEscaped' <- hasEscaped
  hasReachedMaxSteps' <- hasReachedMaxSteps
  hasCollided' <- hasCollided
  if hasEscaped'
    then gets (fog . config)
  else if hasCollided' || hasReachedMaxSteps'
    then getColour
  else step >> raymarcher

getColour :: Raymarcher (A.Exp Colour)
getColour = do
  v1 <- gets (sun . config)
  v2 <- gets (normal . epsilon . config) <*> gets (shape . config) <*> gets positionVector
  lit <- getIsLit
  ambient <- gets (ambientLighting . config)
  let lightingNormal = (v1 `AL.dot` v2) / (AL.norm v1 * AL.norm v2)
      lightingFactor = if lightingNormal < 0 || not lit
        then realToFrac ambient
        else realToFrac $ (1 - ambient)*lightingNormal + ambient
  litBaseColour <- blend (1 - lightingFactor) lightingFactor black <$> getBaseColour
  fogFactor <- (\d maxd -> if d > maxd then 1.0 else realToFrac (d/maxd)) <$> distanceFromStartSq <*> gets (maxDistanceSq . config)
  blend (1 - fogFactor) fogFactor litBaseColour <$> gets (fog . config)

getIsLit :: Raymarcher (A.Exp Bool)
getIsLit = evalState go . f <$> get
  where
    f s = s { directionVector = sun (config s), steps = 0 }
    go = do
      escapeSurface
      step
      hasEscaped' <- hasEscaped
      hasReachedMaxSteps' <- hasReachedMaxSteps
      hasCollided' <- hasCollided
      if hasEscaped'
        then pure (A.constant True)
      else if hasCollided' || hasReachedMaxSteps'
        then pure (A.constant False)
      else step >> go

escapeSurface :: Raymarcher ()
escapeSurface = do
  pos <- gets positionVector
  eps <- gets (epsilon . config)
  dir <- (\s -> normal eps s pos) <$> gets (shape . config)
  dist <- gets (epsilon . config)
  modify $ \s -> s { positionVector = pos + (dir AL.^* dist) }

instance A.Elt a => A.IfThenElse (A.Exp Bool) (Raymarcher (A.Exp a)) where
  ifThenElse :: A.Exp Bool -> Raymarcher (A.Exp a) -> Raymarcher (A.Exp a) -> Raymarcher (A.Exp a)
  ifThenElse p x y = do
    startState <- get
    let (resultx, endStatex) = runState x startState
        (resulty, endStatey) = runState y startState
        -- TODO: clean this up
        result = A.cond p resultx resulty
        endSteps = A.cond p (steps endStatex) (steps endStatey)
        endDirection = A.cond p (directionVector endStatex) (directionVector endStatey)
        endPosition = A.cond p (positionVector endStatex) (positionVector endStatey)
        -- we assume config has not been modified
    put $ startState{ steps = endSteps, directionVector = endDirection, positionVector = endPosition }
    pure result
