{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PatternSynonyms #-}
module Main (main) where

import qualified Linear as L
import Data.Array.Accelerate.Linear ()
import Graphics.Gloss.Interface.Pure.Game (Event (EventKey), Key (..), KeyState (..), SpecialKey (..))
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Array.Accelerate.Interpreter as Interp
import qualified Data.Array.Accelerate as A
import Graphics.Gloss.Accelerate.Raster.Field
import GHC.Generics (Generic)
import Data.List (singleton)
import World.Shape (Shape)
import World (yellowSphereOnPlaneGreenCube)
import Raymarch
import qualified Data.Array.Accelerate.Linear as AL
import Data.Array.Accelerate.Data.Colour.Names

windowWidth, windowHeight :: Int
windowWidth = 600
windowHeight = 600

window :: Display
window = InWindow "Raymarcher" (windowWidth, windowHeight) (0, 0)

data CameraState = CameraState
  { cameraPosition :: L.V3 Double
  , cameraRotation :: Double
  , cameraSun :: Double
  , pressedKeys :: S.Set Key
  }

initialCameraState :: CameraState
initialCameraState = CameraState
  { cameraPosition = L.V3 0 0 0
  , cameraRotation = 0
  , cameraSun = 0
  , pressedKeys = S.empty
  }

cameraDirection :: CameraState -> L.V3 Double
cameraDirection CameraState{cameraRotation} = L.rotate (L.axisAngle (L.V3 0 1 0) cameraRotation) (L.V3 0 0 1)

cameraSunDirection :: CameraState -> L.V3 Double
cameraSunDirection CameraState{cameraSun} = L.rotate (L.axisAngle (L.V3 0 0 1) cameraSun) (L.normalize $ L.V3 1 0.5 0)

prepareState :: CameraState -> AState
prepareState state@CameraState{cameraPosition, cameraRotation} = 
  A.fromList A.Z . singleton $ CameraInfo
    { aCameraPosition = cameraPosition
    , aCameraRotation = cameraRotation
    , aCameraSun = cameraSunDirection state
    }

type AState = A.Scalar CameraInfo

data CameraInfo = CameraInfo
  { aCameraPosition :: L.V3 Double
  , aCameraRotation :: Double
  , aCameraSun :: L.V3 Double
  } deriving (Show, Eq, Generic, A.Elt )

pattern CameraInfo_ :: A.Exp (L.V3 Double) -> A.Exp Double -> A.Exp (L.V3 Double) -> A.Exp CameraInfo
pattern CameraInfo_ pos rot sun = A.Pattern (pos,rot,sun)
{-# COMPLETE CameraInfo_ #-}

main :: IO ()
main = do
  playFieldWith Interp.run1 window (7,7) 15 initialCameraState prepareState aGetColourAtPoint handleKeys update

aGetColourAtPoint :: A.Acc AState -> A.Exp (L.V2 Float) -> A.Exp Colour
aGetColourAtPoint aState (AL.V2_ x y) =
  let (CameraInfo_ pos rot sun) = A.the aState
      config = Config
        { maxSteps = 100
        , maxDistanceSq = 100
        , epsilon = 0.001
        , initialDirectionVector = AL.rotate 
            (AL.axisAngle (A.lift $ L.V3 (0 :: A.Exp Double) 1 0) rot) 
            (AL.normalize (A.lift $ L.V3 (A.toFloating x :: A.Exp Double) (A.toFloating (-y)) 1))
        , initialPositionVector = pos
        , fog = black
        , sun
        , shape = scene aState
        , ambientLighting = 0.1
        }
   in runRaymarcher config

scene :: A.Acc AState -> Shape
scene = const yellowSphereOnPlaneGreenCube

handleKeys :: Event -> CameraState -> CameraState
handleKeys (EventKey key Down _ _) state = state { pressedKeys = S.insert key (pressedKeys state) }
handleKeys (EventKey key Up _ _) state = state { pressedKeys = S.delete key (pressedKeys state) }
handleKeys _ state = state

movementKeybindings :: M.Map Key (CameraState -> CameraState)
movementKeybindings = M.fromList [(Char 'w', \state -> state { cameraPosition = cameraPosition state + cameraDirection state L.^* 0.5 })
                                 ,(Char 's', \state -> state { cameraPosition = cameraPosition state - cameraDirection state L.^* 0.5 })
                                 ,(Char 'a', \state -> state { cameraPosition = cameraPosition state - L.cross (L.V3 0 1 0) (cameraDirection state) L.^* 0.5 })
                                 ,(Char 'd', \state -> state { cameraPosition = cameraPosition state + L.cross (L.V3 0 1 0) (cameraDirection state) L.^* 0.5 })
                                 ,(Char 'r', \state -> state { cameraPosition = cameraPosition state + L.V3 0 0.5 0 })
                                 ,(Char 'f', \state -> state { cameraPosition = cameraPosition state - L.V3 0 0.5 0 })
                                 ,(Char 'h', \state -> state { cameraRotation = cameraRotation state - 0.1 })
                                 ,(Char 'l', \state -> state { cameraRotation = cameraRotation state + 0.1 })
                                 ,(SpecialKey KeyLeft, \state -> state { cameraSun = cameraSun state + 0.1 })
                                 ,(SpecialKey KeyRight, \state -> state { cameraSun = cameraSun state - 0.1 })
                                 ]

update :: Float -> CameraState -> CameraState
update _ = handleMove

handleMove :: CameraState -> CameraState
handleMove state@CameraState{pressedKeys} =
  let f = S.foldr (\key f' -> fromMaybe id (M.lookup key movementKeybindings) . f') id pressedKeys
   in f state