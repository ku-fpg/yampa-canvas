{-# LANGUAGE OverloadedStrings, TupleSections #-}

module DropBalls where

-- set browser to: http://localhost:3000/
import Graphics.Blank hiding (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Text(Text)

import FRP.Yampa.Canvas
import BouncingBalls hiding (main)

---------------------------------------------------

main :: IO ()
main = blankCanvas 3000 animateDroppingBalls

-- | Display an animation of multiple falling balls.
animateDroppingBalls :: DeviceContext -> IO ()
animateDroppingBalls = reactimateSFinContext clickEvent renderScene dropBalls

---------------------------------------------------

-- Note:
--   data Event a = NoEvent | Event a

clickEvent = const NoEvent -- TODO

-- | Convert a Blank Canvas co-ordinate into a Yampa Position.
toXYCo :: (Float,Float) -> Canvas Position
toXYCo (i,j) =
  do context <- myCanvasContext
     let w = width context
         h = height context
     return $ vector2 (i / w) (1 - j / h)

---------------------------------------------------

-- Construct a bouncing ball model that allows responds to input events by adding new balls.
dropBalls :: SF (Event Position) [Ball]
dropBalls = ballGenerator >>> ballCollection []

ballCollection :: [SF (Event Ball) Ball] -> SF (Event Ball) [Ball]
ballCollection bs = notYet >>> pSwitchB bs (arr fst) (\ sfs b -> ballCollection (bouncingBall1 b : sfs))

-- Convert events carrying co-ordinates into events carrying new "bouncing ball" signal functions.
ballGenerator :: SF (Event Position) (Event Ball)
ballGenerator = arr $ fmap newBallAt

-- Create a new ball of the specified colour, at the specified co-ordinates.
newBallAt :: Position -> Ball
newBallAt p = MkBall
          { pos    = p,
            vel    = vector2 0 0,
            radius = 0.02,
            colour = "purple"
          }

-------------------------------------------------------------------
