module Main (main) where

import Graphics.Gloss.Raster.Field
import World
import World.Shape (Shape)
import Raymarch (runRaymarcher, Config(..))

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
  , initialDirectionVector = (realToFrac x, realToFrac y, 1)
  , initialPositionVector = (0, 0, 0)
  , fog = const black
  , sun = (0, 0, 1)
  , shape = shape
  }

