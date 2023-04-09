module Main (main) where

import Graphics.Gloss.Raster.Field
import World
import World.Shape (Shape)
import Raymarch (runRaymarcher, Config(..))
import qualified Linear as L

windowWidth, windowHeight :: Int
windowWidth = 800
windowHeight = 800

window :: Display
window = InWindow "Raymarcher" (windowWidth, windowHeight) (0, 0)

main :: IO ()
main = 
  playField window (1,1) 1 yellowSphereOnPlane getColourAtPoint (const id) (const id)

getColourAtPoint :: Shape -> Point -> Color
getColourAtPoint shape (x, y) = runRaymarcher Config 
  { maxSteps = 100
  , maxDistanceSq = 100
  , epsilon = 0.001
  , initialDirectionVector = L.V3 (realToFrac x) (realToFrac y) 1
  , initialPositionVector = L.V3 0 0 0
  , fog = const black
  , sun = L.V3 0 0 1
  , shape = shape
  }

