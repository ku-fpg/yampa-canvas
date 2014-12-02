{-# LANGUAGE OverloadedStrings #-}
import Prelude hiding (Left, Right)
import Graphics.Blank hiding (Event)
import qualified Graphics.Blank as Blank
import FRP.Yampa
import FRP.Yampa.Vector2 -- 2d abstraction

import FRP.Yampa.Canvas

type Position = Vector2 Double
type Velocity = Vector2 Double
type World    = (Paddle, Paddle, Ball)

data KeyPress  = Pressed (Dir, Dir) | Released (Dir, Dir) 
data Dir       = Horizontal | Vertical | Up | Down | Left | Right | None
data Ball      = Ball Position Velocity    
data Paddle    = Paddle Double Dir

radius :: Double
radius = 0.008

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
  let mkKey ds = case eType ev of
                   "keydown" -> Event $ Pressed  ds
                   "keyup"   -> Event $ Released ds in
  case eWhich ev of
        Nothing -> return NoEvent
        Just k  -> return $ case k of 
                              87 -> mkKey (Left, Up)
                              83 -> mkKey (Left, Down)
                              38 -> mkKey (Right, Up)
                              40 -> mkKey (Right, Down)
                              _  -> NoEvent 
   

      
-- processes 

process :: SF (Event KeyPress) World 
process = loopPre startWorld $ 
            (arr $ uncurry movePaddles) >>> 
            moveBall >>> 
            detectCollision >>> 
            (arr redirect) >>> (identity &&& identity)

mkPaddle :: Double -> Dir -> Paddle 
mkPaddle y None = Paddle y None
mkPaddle y Up   = if y >= 1 - pHeight then Paddle (1.0 - pHeight) Up else Paddle (y+0.015) Up
mkPaddle y Down = if y <= 0 then Paddle 0 Down else Paddle (y-0.015) Down

movePaddles :: (Event KeyPress) -> World -> World
movePaddles NoEvent w = w
movePaddles (Event (Released (Left, _))) (Paddle y1 _, Paddle y2 d2, b) = 
  (mkPaddle y1 None, mkPaddle y2 d2, b)
movePaddles (Event (Released (Right, _))) (Paddle y1 d1, Paddle y2 _, b) =
  (mkPaddle y1 d1, mkPaddle y2 None, b) 
movePaddles (Event (Pressed k)) (Paddle y1 d1, Paddle y2 d2, b) =
  let
      (d1', d2') = case k of 
                     (Left, Up)    -> (Up, d2) 
                     (Left, Down)  -> (Down, d2) 
                     (Right, Up )  -> (d1, Up) 
                     (Right, Down) -> (d1, Down) 
   in
   (mkPaddle y1 d1', mkPaddle y2 d2', b)


moveBall :: SF World World
moveBall = arr move where
  move (p1, p2, Ball p v) = let (x ,  y) = vector2XY p
                                (dx, dy) = vector2XY v in
                            (p1, p2, Ball (vector2 (x+dx) (y+dy)) v)

redirect :: (World, Event Dir) -> World 
redirect (w, NoEvent) = w
redirect ((p1@(Paddle y1 _ ), p2@(Paddle y2 _ ), Ball p v), Event d) = 
  case d of
    Vertical -> (p1, p2, Ball p (vector2 dx (-dy)))
    Left     -> (p1, p2, Ball p (vector2 (newX y1) (newY y1)))
    Right    -> (p1, p2, Ball p (vector2 (newX y2) (newY y2)))
    None     -> (p1, p2, ball) 
  where
    (dx, dy)      = vector2XY v 
    ( x,  y)      = vector2XY p
    newY yp = if y > yp + pHeight / 2 
              then (y - (yp + pHeight / 2)) * 0.02 + dy 
              else ((yp + pHeight / 2) - y) * 0.02 + dy
    newX yp = let dx' = (y - (yp + pHeight / 2)) * 0.02 + dx  in
              if (dx >= 0 && dx' >= 0) || (dx < 0 && dx' < 0 ) then (-dx') else dx'
    
     
   

detectCollision :: SF World (World, Event Dir) 
detectCollision = arr detect where
  detect w@(Paddle y1 _, Paddle y2 _, Ball p v) 
    |yb + radius >= 1 || yb - radius <= 0 = (w, Event Vertical) 
    |xb - radius <= 0 || xb + radius >= 1 = (w, Event None) -- resets the ball, no scoring yet
    |(xb - radius <= pOffset && xb >= pOffset && yb + radius >= y1 && yb - radius <= y1 + pHeight) = (w, Event Left)
    | (xb + radius >= 1 - pOffset && xb <= 1 - pOffset && yb + radius >= y2 && yb - radius <= y2 + pHeight) = (w, Event Right) 
    | otherwise = (w, NoEvent)
    where
      (xb, yb) = vector2XY p 


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
 
