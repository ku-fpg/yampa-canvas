{-# LANGUAGE OverloadedStrings #-}
{- 
   Simple pong implementation using Yampa and Blank Canvas. 
   This could possibly be simplified or made more idiomatic in the future
-}
import Prelude hiding (Left, Right)
import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank
import FRP.Yampa
import FRP.Yampa.Vector2

import FRP.Yampa.Canvas

type Position    = Vector2 Double
type Velocity    = Vector2 Double
type World       = (Paddle, Paddle, Ball) -- this might be better a its own data type, but this was a simple setup
type Reset a b c = c -> (a -> Maybe b)

data KeyPress  = Pressed PaddleID Dir | Released PaddleID Dir 
data PaddleID  = Left | Right
data Dir       = Up | Down | None
data Ball      = Ball Position Velocity    
data Paddle    = Paddle Double Dir

-- many game constants
radius :: Double
radius = 0.008

maxVelocity :: Double
maxVelocity = 0.0035 

pHeight :: Double
pHeight = 0.2

pWidth :: Double
pWidth = 0.015

pOffset :: Double
pOffset = 0.1

ball :: Ball
ball = Ball (vector2 0.5 0.5) (vector2 0.0015 0.00)

startWorld :: World
startWorld = (Paddle (0.5 - pHeight / 2) None, Paddle (0.5 - pHeight / 2) None, ball)

main :: IO ()
main = blankCanvas 3000 { events = ["keydown", "keyup"] } $ animate 

animate :: DeviceContext -> IO ()
animate = reactimateSFinContext detectKeyPress renderGame process

-- input 

detectKeyPress :: Blank.Event -> Canvas (Event KeyPress)
detectKeyPress ev =
  let mkKey id d = case eType ev of
                   "keydown" -> Event $ Pressed  id d
                   "keyup"   -> Event $ Released id d in
  case eWhich ev of
        Nothing -> return NoEvent
        Just k  -> return $ case k of 
                              87 -> mkKey Left  Up
                              83 -> mkKey Left  Down
                              38 -> mkKey Right Up
                              40 -> mkKey Right Down
                              _  -> NoEvent 
   
-- processes 

process :: SF (Event KeyPress) World 
process = loopPre startWorld $ 
            (arr $ uncurry movePaddles) >>> 
            arr moveBall                >>> 
            handleCollisions            >>> 
            (identity &&& identity)
            

mkPaddle :: Double -> Dir -> Paddle 
mkPaddle y None = Paddle y None
mkPaddle y Up   = if y >= 1 - pHeight then Paddle (1.0 - pHeight) Up else Paddle (y+0.0025) Up
mkPaddle y Down = if y <= 0 then Paddle 0 Down else Paddle (y-0.0025) Down

movePaddles :: (Event KeyPress) -> World -> World
movePaddles NoEvent (Paddle y1 d1, Paddle y2 d2, b) = 
  (mkPaddle y1 d1, mkPaddle y2 d2, b)
movePaddles (Event (Released Left _)) (Paddle y1 _, Paddle y2 d2, b) = 
  (mkPaddle y1 None, mkPaddle y2 d2, b)
movePaddles (Event (Released Right _)) (Paddle y1 d1, Paddle y2 _, b) =
  (mkPaddle y1 d1, mkPaddle y2 None, b) 
movePaddles (Event p) (Paddle y1 d1, Paddle y2 d2, b) =
  let
      (d1', d2') = case p of 
                     Pressed Left  Up   -> (Up, d2) 
                     Pressed Left  Down -> (Down, d2) 
                     Pressed Right Up   -> (d1, Up) 
                     Pressed Right Down -> (d1, Down) 
   in
   (mkPaddle y1 d1', mkPaddle y2 d2', b)


moveBall :: World -> World
moveBall (p1, p2, Ball p v) = (p1, p2, Ball (vector2 (x+dx) (y+dy)) v) 
  where 
    (x ,  y) = vector2XY p
    (dx, dy) = vector2XY v 


handleCollisions :: SF World World
handleCollisions = returnBall >>> bounceBall >>> deflectBall

-- handles vertical boundaries
bounceBall :: SF World World
bounceBall = resetSwitch (edgeWhen detectWall) redirect 
  where
    redirect :: () -> (World -> Maybe World)
    redirect _ = (\w@(p1, p2, Ball p v) -> 
                     let (dx, dy) = vector2XY v in
                     if detectWall w
                     then Just (p1, p2, Ball p (vector2 dx (-dy)))
                     else Nothing)

-- handles horizontal boundaries
returnBall :: SF World World 
returnBall = resetSwitch (edgeWhen detectScore) restart
  where
    restart :: () -> (World -> (Maybe World))
    restart _ = (\w@(p1, p2, Ball _ v) -> 
                    let (dx, dy) = vector2XY v
                        dx' = if dx < 0 then 0.0015 else -0.0015 
                    in 
                    if detectScore w
                    then Just (p1, p2, Ball (vector2 0.5 0.5) (vector2 dx' 0))
                    else Nothing) 
      
-- handles paddle collisions
deflectBall :: SF World World
deflectBall = 
  resetSwitch (edgeWhen $ detectPaddle Left) (deflect Left) >>> resetSwitch (edgeWhen $ detectPaddle Right) (deflect Right)
  where
    deflect :: PaddleID -> () -> (World -> Maybe World)
    deflect dir _  = 
      (\w@(Paddle y1 d1, Paddle y2 d2, Ball p v) ->
          let 
          (dx, dy)  = vector2XY v
          ( x,  y)  = vector2XY p
          distance yp = y - (yp + pHeight / 2)
          newY yp = distance yp / (pHeight / 2 ) * maxVelocity 
          newX yp = let dx' = if distance yp >= 0 
                              then maxVelocity - maxVelocity * (distance yp) /  pHeight  
                              else maxVelocity - maxVelocity * (-distance yp) / pHeight in
                    if (dx >= 0 && dx' >= 0) || (dx < 0 && dx' < 0 ) then -dx' else dx'
          in
          case dir of
            Left  -> if detectPaddle Left w
                     then Just (Paddle y1 d1, Paddle y2 d2, Ball (vector2 (x + radius) y) $ vector2 (newX y1) (newY y1))
                     else Nothing
            Right -> if detectPaddle Right w
                     then Just (Paddle y1 d1, Paddle y2 d2, Ball (vector2 (x - radius) y) $ vector2 (newX y2) (newY y2))
                     else Nothing)
    
-- collision predicates
     
detectPaddle :: PaddleID -> World -> Bool
detectPaddle dir (Paddle y1 _, Paddle y2 _, Ball p _) =
  case dir of
    Left  -> (xb - radius <= pOffset && xb >= pOffset && yb + radius >= y1 && yb - radius <= y1 + pHeight) 
    Right -> (xb + radius >= 1 - pOffset && xb <= 1 - pOffset && yb + radius >= y2 && yb - radius <= y2 + pHeight) 
  where
    (xb, yb) = vector2XY p


detectWall :: World -> Bool
detectWall (_, _, Ball p _) = yb + radius >= 1 || yb - radius <= 0
  where (xb, yb) = vector2XY p

detectScore :: World -> Bool
detectScore (_, _, Ball p _) = xb - radius <= 0 || xb + radius >= 1
  where (xb, yb) = vector2XY p

-- rendering functions

renderGame :: World -> Canvas ()
renderGame (p1, p2, b) = scaleScene >> drawBall b >> drawPaddle p1 pOffset >> drawPaddle p2 (1-pOffset-pWidth)

drawPaddle :: Paddle -> Double -> Canvas ()
drawPaddle (Paddle y _) offset = 
  do 
  fillStyle("red")
  fillRect(offset, y, pWidth, pHeight)

drawBall :: Ball -> Canvas ()
drawBall (Ball p _) = 
  do
  let (x,y) = vector2XY p
  beginPath()
  arc(x, y, radius, 0, pi*2, False)
  closePath()
  fillStyle "yellow"
  fill()

scaleScene :: Canvas ()
scaleScene =
   do context <- myCanvasContext
      let w = width context
          h = height context
      translate (0,h)
      scale (w, negate h)
 
-- auxillary functions

-- | This can be thought of as a correction switch. Whenever an event occurs in sf it uses the contents 
-- | of the event to generate a correcting signal (such as moving an out of bounds objects to a correct position)
-- | When this signal is done performing its correction the switch returns to the original function and once again awaits correction. 
-- | The reset type may very well be better defined as a type synonym over SF rather than c -> a -> Maybe b, but this was a simple
-- | general first implementation
resetSwitch :: SF a (b, Event c) -> Reset a b c -> SF a b
resetSwitch sf rf = switch sf (\c -> switch (rf' c) $ \_ -> resetSwitch sf rf)
  where
    rf' c = arr (\a -> case rf c a of
                        Nothing -> (undefined, Event () )
                        Just b  -> (b, NoEvent))
  
edgeWhen :: (World -> Bool) -> SF World (World, Event ())
edgeWhen p = identity &&& (arr p >>> iEdge False)
