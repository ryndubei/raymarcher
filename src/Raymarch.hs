{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
module Raymarch (runRaymarcher, Config(..)) where

import Prelude hiding ((||), (<), (>), (&&), not)
import World.Shape
import qualified Data.Array.Accelerate.Linear as AL
import Data.Array.Accelerate.Linear ()
import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (ifThenElse, (||), (<), (>), not)
import Data.Array.Accelerate.Data.Colour.RGBA (Colour, blend)
import Data.Array.Accelerate.Data.Colour.Names (black)
import Control.Monad.RWS.Lazy

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
  , positionVector :: A.Exp Point
  , steps :: A.Exp Int
  } 

type Raymarcher = RWS Config () RaymarcherState

evalRaymarcher :: Raymarcher a -> Config -> RaymarcherState -> a
evalRaymarcher m c = fst . evalRWS m c

execRaymarcher :: Raymarcher a -> Config -> RaymarcherState -> RaymarcherState
execRaymarcher m c = fst . execRWS m c

initialRaymarcherState :: Config -> RaymarcherState
initialRaymarcherState config' = RaymarcherState
  { directionVector = initialDirectionVector config'
  , positionVector = initialPositionVector config'
  , steps = 0
  }

config :: (Config -> a) -> Raymarcher a
config = asks

getConfig :: Raymarcher Config
getConfig = ask

distanceFromStartSq :: Raymarcher (A.Exp Double)
distanceFromStartSq = do
  pos <- config initialPositionVector 
  pos' <- gets positionVector
  pure $ AL.quadrance (pos' - pos)

hasEscaped :: Raymarcher (A.Exp Bool)
hasEscaped = do
  maxd <- config maxDistanceSq
  (maxd <) <$> distanceFromStartSq

hasReachedMaxSteps :: Raymarcher (A.Exp Bool)
hasReachedMaxSteps = do
  maxs <- config maxSteps
  (maxs <) <$> gets steps

hasCollided :: Raymarcher (A.Exp Bool)
hasCollided = do
  signedDistance <- config (distance . shape) <*> gets positionVector
  (signedDistance <) <$> config epsilon

step :: Raymarcher ()
step = do
  pos <- gets positionVector
  dir <- gets directionVector
  dist <- config (distance . shape) <*> gets positionVector
  modify $ \s -> s { positionVector = pos + (dir AL.^* dist), steps = steps s + 1 }

getBaseColour :: Raymarcher (A.Exp Colour)
getBaseColour = config (colour . shape) <*> gets positionVector

runRaymarcher :: Config -> A.Exp Colour
runRaymarcher config' = evalRaymarcher raymarcher config' (initialRaymarcherState config')

has :: Raymarcher (A.Exp Bool)
has = foldr1 (||) <$> sequence [hasEscaped, hasReachedMaxSteps, hasCollided]

raymarcher :: Raymarcher (A.Exp Colour)
raymarcher = do
  configFog <- config fog
  whileState (not <$> has) step
  hasEscaped' <- hasEscaped
  col <- getColour
  pure $ if hasEscaped'
    then configFog
  else col

getColour :: Raymarcher (A.Exp Colour)
getColour = do
  v1 <- config sun
  v2 <- config (normal . epsilon) <*> config shape <*> gets positionVector
  ambient <- config ambientLighting
  lit <- getIsLit
  let lightingNormal = (v1 `AL.dot` v2) / (AL.norm v1 * AL.norm v2)
      lightingFactor = if lightingNormal < 0 || not lit
        then A.toFloating ambient
        else A.toFloating $ (1 - ambient)*lightingNormal + ambient
  litBaseColour <- blend (1 - lightingFactor) lightingFactor black <$> getBaseColour
  fogFactor <- (\d maxd -> if d > maxd then 1.0 else A.toFloating (d/maxd)) <$> distanceFromStartSq <*> config maxDistanceSq
  blend (1 - fogFactor) fogFactor litBaseColour <$> config fog

getIsLit :: Raymarcher (A.Exp Bool)
getIsLit = do
  sun' <- config sun
  s' <- gets (\s -> s { directionVector = sun', steps = 0})
  config' <- getConfig
  pure $ evalRaymarcher go config' s'
  where
    go = do
      escapeSurface
      step
      whileState (not <$> has) step
      hasEscaped' <- hasEscaped
      pure $ if hasEscaped'
        then A.constant True
      else A.constant False

escapeSurface :: Raymarcher ()
escapeSurface = do
  pos <- gets positionVector
  eps <- config epsilon
  shape' <- config shape
  let dir = normal eps shape' pos
  modify $ \s -> s { positionVector = pos + (dir AL.^* eps) }

-- | Given a monadic predicate on the state, and a monadic action,
-- execute the action repeatedly while the predicate holds.
-- Any modifications on the state by the predicate are ignored.
whileState :: Raymarcher (A.Exp Bool) -> Raymarcher () -> Raymarcher ()
whileState p m = do
  s <- get
  config' <- getConfig
  let p' (A.T3 pos dir steps) = evalRaymarcher p config' s{ positionVector = pos, directionVector = dir, steps}
      s' = A.lift $ simplify s
      executeOnce (A.T3 pos dir steps) =
        let s'' = s{ positionVector = pos, directionVector = dir, steps = steps }
         in A.lift . simplify $ execRaymarcher m config' s''
      (A.T3 endPos endDir endSteps) = A.while p' executeOnce s'
  put s{ positionVector = endPos, directionVector = endDir, steps = endSteps }
  where
    simplify s = (positionVector s, directionVector s, steps s)
