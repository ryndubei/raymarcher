{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
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

prepareState :: CameraState -> AState
prepareState state@CameraState{cameraPosition, cameraSun} = 
  A.fromList A.Z . singleton $ CameraInfo
    { aCameraPosition = cameraPosition
    , aCameraDirection = cameraDirection state
    , aCameraSun = cameraSun
    }

type AState = A.Scalar CameraInfo

data CameraInfo = CameraInfo
  { aCameraPosition :: L.V3 Double
  , aCameraDirection :: L.V3 Double
  , aCameraSun :: Double
  } deriving (Show, Eq, Generic, A.Elt)

main :: IO ()
main = do
  playFieldWith Interp.run1 window (7,7) 15 initialCameraState prepareState aGetColourAtPoint handleKeys update

aGetColourAtPoint :: A.Acc AState -> A.Exp (L.V2 Float) -> A.Exp Colour
aGetColourAtPoint = undefined

-- plan: as we cannot pass a function to Accelerate, we will use this instead
-- to generate the distance function representing the shape inside Accelerate
-- this will be called by aGetColourAtPoint
scene :: A.Acc AState -> A.Exp Shape
scene = undefined

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