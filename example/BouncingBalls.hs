{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module BouncingBalls where

-- set browser to: http://localhost:3000/
import Graphics.Blank hiding (Event)
import FRP.Yampa
import FRP.Yampa.Vector2
import Data.Text(Text)

import FRP.Yampa.Canvas

---------------------------------------------------

main :: IO ()
main = blankCanvas 3000 animateBouncingBalls

-- | Display an animation of multiple falling balls.
animateBouncingBalls :: DeviceContext -> IO ()
animateBouncingBalls = reactimateSFinContext (\ _ _ -> ()) renderScene (bouncingBalls someBalls)

---------------------------------------------------

-- We scale such that 0.0 and 1.0 denote the edges of the canvas.
type Radius = Float

type Acceleration = Vector2 Float
type Velocity     = Vector2 Float
type Position     = Vector2 Float

type Colour = Text

---------------------------------------------------

data Ball = MkBall { pos    :: Position,
                     vel    :: Velocity,
                     radius :: Radius,
                     colour :: Colour
                   }

negateXVel :: Ball -> Ball
negateXVel b = b { vel = let (x,y) = vector2XY (vel b)
                          in vector2 (negate x) y
                 }

negateYVel :: Ball -> Ball
negateYVel b = b { vel = let (x,y) = vector2XY (vel b)
                          in vector2 x (negate y)
                 }

---------------------------------------------------

ball1 :: Ball
ball1 = MkBall { pos    = vector2 0.1 0.5,
                 vel    = vector2 0.4 0.1,
                 radius = 0.05,
                 colour = "red"
               }

ball2 :: Ball
ball2 = MkBall { pos    = vector2 0.3 0.7,
                 vel    = vector2 (-0.5) (-0.1),
                 radius = 0.03,
                 colour = "blue"
               }

ball3 :: Ball
ball3 = MkBall { pos    = vector2 0.5 0.4,
                 vel    = vector2 0.2 1.3,
                 radius = 0.04,
                 colour = "green"
               }

ball4 :: Ball
ball4 = MkBall { pos    = vector2 0.7 0.8,
                 vel    = vector2 (-0.1) (-0.2),
                 radius = 0.06,
                 colour = "yellow"
               }

ball5 :: Ball
ball5 = MkBall { pos    = vector2 0.2 0.9,
                 vel    = vector2 1.8 0.85,
                 radius = 0.02,
                 colour = "orange"
               }

someBalls :: [Ball]
someBalls = [ball1,ball2,ball3,ball4,ball5]

gravity :: Acceleration
gravity = vector2 0 (-1)

---------------------------------------------------

-- | Construct a free-falling ball from an initial ball configuration.
fallingBall :: Ball -> SF x Ball
fallingBall b = constant gravity >>> accelerator (vel b) (pos b) >>> arr updateBall
  where
    updateBall :: (Position,Velocity) -> Ball
    updateBall (p,v) = b { pos = p, vel = v }

-- | Construct a ball that bounces in one dimension from an initial ball configuration.
bouncingBall1 :: forall x. Ball -> SF x Ball
bouncingBall1 b = switchWhen (fallingBall b) detectFloor f
  where
    detectFloor :: SF Ball (Event Ball)
    detectFloor = edgeWhen p
      where
        p :: Ball -> Bool
        p b1 = (vector2Y (pos b1) <= radius b1) || (vector2Y (pos b1) >= 1 - radius b1)

    f :: Ball -> SF x Ball
    f b2 = bouncingBall1 (negateYVel b2)

-- | Construct a ball that bounces in two dimensions.
bouncingBall2 :: forall x. Ball -> SF x Ball
bouncingBall2 b = switchWhen (bouncingBall1 b) detectWall f
  where
    detectWall :: SF Ball (Event Ball)
    detectWall = edgeWhen p
      where
        p :: Ball -> Bool
        p b1 = (vector2X (pos b1) <= radius b1) || (vector2X (pos b1) >= 1 - radius b1)

    f :: Ball -> SF x Ball
    f b2 = bouncingBall2 (negateXVel b2)

-- | Construct a list of bouncing balls from a list of initial ball configurations.
bouncingBalls :: [Ball] -> SF x [Ball]
bouncingBalls bs = parB (map bouncingBall2 bs)

-------------------------------------------------------------------

-- | Draw a circle.
circle :: Position -> Radius -> Colour -> Canvas ()
circle pos' r col = do beginPath ()
                       arc (vector2X pos', vector2Y pos', r, 0, pi*2, False)
                       closePath ()
                       fillStyle col
                       fill ()

-- | A Canvas action to render a single Ball.
renderBall :: Ball -> Canvas ()
renderBall b = circle (pos b) (radius b) (colour b)

-- | A Canvas action to render a list of Balls.
renderBalls :: [Ball] -> Canvas ()
renderBalls = mapM_ renderBall

-- | A Canvas action to render the entire scene.
renderScene :: [Ball] -> Canvas ()
renderScene bs = scaleScene >> renderBalls bs

-- | We scale such that (0,0) is the bottom-left of the canvas and (1,1) is the top-right.
scaleScene :: Canvas ()
scaleScene =
   do context <- myCanvasContext
      let w = width context
          h = height context
      translate (0,h)
      scale (w, negate h)

-------------------------------------------------------------------

-- Auxillary signal function combinators not provided by the Yampa library.

-- | Given an initial 'Velocity' and 'Position', produce a signal function that takes an acceleration and emits
--   the current 'Velocity' and 'Distance'.
accelerator :: Velocity -> Position -> SF Acceleration (Position,Velocity)
accelerator v0 d0 = imIntegral v0 >>> (imIntegral d0 &&& identity)

-- | A variant of 'FRP.Yampa.switch' where the event only depends on the output of the sub-ordinate signal function.
switchWhen :: SF a b -> SF b (Event e) -> (e -> SF a b) -> SF a b
switchWhen sf1 sfe = switch (sf1 >>> identity &&& sfe)

-- | A variant of 'FRP.Yampa.edge' which takes a predicate on an input signal, and tags the event with the value of the input signal.
edgeWhen :: (a -> Bool) -> SF a (Event a)
edgeWhen p = (p ^>> edge) &&& identity >>^ uncurry tag

-------------------------------------------------------------------
