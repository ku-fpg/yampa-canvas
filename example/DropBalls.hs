{-# LANGUAGE OverloadedStrings, TupleSections #-}

module Main where

-- set browser to: http://localhost:3000/
import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank
import FRP.Yampa
import FRP.Yampa.Vector2

import FRP.Yampa.Canvas
import BouncingBalls hiding (main,renderScene)

---------------------------------------------------

main :: IO ()
main = blankCanvas 3000 { events = ["click"] } $ animateDroppingBalls

-- | Display an animation of multiple falling balls.
animateDroppingBalls :: DeviceContext -> IO ()
animateDroppingBalls = reactimateSFinContext detectClick renderScene dropBalls

-- | A Canvas action to render the entire scene.
renderScene :: [Ball] -> Canvas ()
renderScene bs = renderInstructions >> scaleScene >> renderBalls bs

-- | A Canvas action to render the instruction text.
renderInstructions :: Canvas ()
renderInstructions =
 do font "20pt Comic Sans MS"
    fillText ("Click on the canvas to add a ball",50,50)

---------------------------------------------------

-- | Detect a mouse click in the canvas.
detectClick :: Blank.Event -> Canvas (Event Position)
detectClick ev = case ePageXY ev of
                   Nothing     -> return NoEvent
                   Just (x,y)  -> fmap Event (toXYCo (x,y))

-- | Convert a Blank Canvas co-ordinate into a Yampa Position.
toXYCo :: (Float,Float) -> Canvas Position
toXYCo (i,j) =
  do context <- myCanvasContext
     let w = width context
         h = height context
     return $ vector2 (i / w) (1 - j / h)

---------------------------------------------------

-- | Construct a bouncing ball model that allows responds to input events by adding new balls.
dropBalls :: SF (Event Position) [Ball]
dropBalls = ballGenerator >>> ballCollection []

-- | A collection of active balls into which new balls can be dropped.
ballCollection :: [SF (Event Ball) Ball] -> SF (Event Ball) [Ball]
ballCollection bs = notYet >>> pSwitchB bs (arr fst) (\ sfs b -> ballCollection (bouncingBall1d b : sfs))

-- | Convert events carrying co-ordinates into events carrying new balls.
ballGenerator :: SF (Event Position) (Event Ball)
ballGenerator = arr (fmap newBallAt)

-- | Create a new ball of the specified colour, at the specified co-ordinates.
newBallAt :: Position -> Ball
newBallAt p = MkBall
          { pos    = p,
            vel    = vector2 0 0,
            radius = 0.02,
            colour = "purple"
          }

-------------------------------------------------------------------
