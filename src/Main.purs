module Main where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Int (parity)
import Data.Maybe (Maybe(..))
import FFI.Util (property, setProperty)
import Graphics.Canvas (CANVAS, CanvasElement, Context2D, beginPath, clearRect, closePath, getCanvasElementById, getContext2D, lineTo, moveTo, rotate, scale, setCanvasHeight, setCanvasWidth, stroke, translate)
import Math (cos, sin, pi)
import Partial.Unsafe (unsafePartial)

--foreign imports
foreign import height ::forall a.Eff(canvas :: CANVAS , console :: CONSOLE | a)Number
foreign import width ::forall a.Eff(canvas :: CANVAS , console :: CONSOLE | a)Number
foreign import  data Event :: Type
foreign import animation :: forall e.Context2D->(Context2D -> Number->  Eff (canvas :: CANVAS | e) Unit)
                -> Eff (canvas :: CANVAS | e) Unit
foreign import addEventListener :: forall a. CanvasElement -> String
                                    -> (Event -> Eff (canvas :: CANVAS | a) Unit)
                                    -> Eff (canvas :: CANVAS | a) Unit



-- newtypes 3d and 2d
newtype P3d = P3d {
  x :: Number
, y :: Number
, z :: Number
}

newtype P2d = P2d {
  x :: Number
, y :: Number
}

newtype Angle = Angle {
  ax :: Number
,  ay :: Number
,  az :: Number
}

--instacnce for p2d
instance p2d' :: Show P2d where
  show (P2d p) = show p.x <> " " <>show p.y

instance p3d' :: Show P3d where
  show (P3d p) = show p.x <> " " <>show p.y <>" "  <> show p.z


rotateX :: P3d -> Number -> P3d
rotateX (P3d vec) angle = do
    let sinA = sin(angle)
    let cosA = cos(angle)
    let rty = vec.y * cosA - vec.z * sinA
    let rtz = vec.y * sinA +vec.z * cosA
    P3d  { x: vec.x, y: rty, z:rtz
    }
rotateY :: P3d -> Number -> P3d
rotateY (P3d vec) angle = do
    let sinA = sin(angle)
    let cosA = cos(angle)
    let rtx = vec.x * cosA + vec.z * sinA
    let rtz = -vec.x * sinA + vec.z * cosA
    P3d { x: rtx, y: vec.y, z:rtz
    }


rotatev :: P3d -> Number -> Number  -> P3d
rotatev (P3d vec) rx ry = rotateY (rotateX (P3d vec) (rad rx)) (rad ry)

rad :: Number -> Number
rad ang = ang * pi /180.0


deltaMove :: P2d
deltaMove = P2d { x: 0.0, y: 0.0}

previous :: P2d
previous = P2d { x: 0.0, y: 0.0}

rotation :: P3d
rotation = P3d { x: 0.0, y: 0.0, z: 0.0}

dragging :: Array Boolean
dragging = [ false ]

keying :: Array Boolean
keying = [ false ]

acceleration :: Array Number
acceleration = [ 0.0 ]



onMouseDown :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseDown evt = void $ do
    _ <- pure $ setProperty dragging "0" true
    pure unit
onMouseUp :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseUp evt = void $ do
    _<- pure $ setProperty dragging "0" false
    pure unit

onMouseMove :: forall e. Event -> Eff (canvas :: CANVAS| e) Unit
onMouseMove evt = void $ do
    let (offSetX :: Number) = property evt "offsetX"
    let (offSetY :: Number) = property evt "offsetY"
    _ <- if(property dragging "0" ) then do
        let (prevMouseX :: Number) = property previous "x"
        let (prevMouseY :: Number) = property previous "y"
        _ <- pure $ setProperty deltaMove "x" (offSetX - prevMouseX)
        _ <- pure $ setProperty deltaMove "y" (offSetY - prevMouseY)
        let accel = (property acceleration "0") + 3.0
        pure $ setProperty acceleration "0" accel
        pure unit
        else do
                pure unit
    _ <- pure $ setProperty previous "x" offSetX
    _ <- pure $ setProperty previous "y" offSetY
    pure unit


-- function to convert 3d point to 2d
convert ::forall a.P3d ->P2d
convert ( P3d p3d) = do
  let p = 300.0
  let z = 1500.0
  let scale  = p /  (p + p3d.z + z)
  -- logShow (scale)
  P2d{x : p3d.x * scale, y : p3d.y * scale}
  

--to drwa lines between two points
draw :: forall a.Context2D->P2d ->P2d->Eff(canvas :: CANVAS | a)Unit
draw ctx (P2d p1) (P2d p2) = void $ do
  void $ beginPath ctx
  void $ moveTo ctx p1.x p1.y
  void $ lineTo ctx p2.x p2.y
  void $ closePath ctx
  stroke ctx


  --cube points
cube ::forall a.Context2D->Angle ->Eff(canvas :: CANVAS, console :: CONSOLE |a) Unit
cube ctx (Angle a) = void $ do
  let p1 = convert $rotatev (P3d({x:(-500.0), y:(-500.0),z :500.0})) a.ax a.ay
  let p2 = convert $rotatev (P3d({x:(500.0), y:(-500.0),z :500.0})) a.ax a.ay
  let p3 = convert $rotatev (P3d({x:(500.0), y:(-500.0),z :(-500.0)})) a.ax a.ay
  let p4 = convert $rotatev (P3d({x:(-500.0), y:(-500.0),z :(-500.0)})) a.ax a.ay
  let p5 = convert $rotatev (P3d({x:(-500.0), y:(500.0),z :500.0})) a.ax a.ay
  let p6 = convert $rotatev (P3d({x:(500.0), y:(500.0),z :500.0})) a.ax a.ay
  let p7 = convert $rotatev (P3d({x:(500.0), y:(500.0),z :(-500.0)})) a.ax a.ay
  let p8 = convert $rotatev (P3d({x:(-500.0), y:(500.0),z :(-500.0)})) a.ax a.ay
  -- logShow (p1)
  -- logShow (p2)
  draw ctx p1 p2
  draw ctx p2 p3
  draw ctx p3 p4
  draw ctx p4 p1
  draw ctx p5 p6
  draw ctx p6 p7
  draw ctx p7 p8
  draw ctx p8 p5
  draw ctx p8 p4
  draw ctx p5 p1
  draw ctx p6 p2
  draw ctx p7 p3
  
max:: Number
max = pi/4.0
may::Number
may=pi/3.0
maz::Number
maz=pi/4.0
acc::Number
acc=0.998
drag::Boolean
drag=false


updateCube :: forall a. Context2D -> Number -> Eff (canvas :: CANVAS , console :: CONSOLE | a) Unit
updateCube ctx timeStamp = void $ do

    let (accel :: Number) = property acceleration "0"
    _ <- if (accel > 0.0) then do
            _ <- pure $ setProperty acceleration "0" (accel - 1.0)
            let (delX :: Number) = property deltaMove "x"
            let (delY :: Number) = property deltaMove "y"
            _ <- pure $ setProperty rotation "x" ((property rotation "x") - delY)
            _ <- pure $ setProperty rotation "y" ((property rotation "y") - delX)
            pure unit

            else do
                pure unit

    let change1 = Angle { ax: property rotation  "x", ay: property rotation  "y", az:0.0 }
    h' <- height
    w' <- width
    -- clear canvas
    void $ clearRect ctx {x:(-w'/2.0),y:(-h'/2.0),w:w',h:h'}
    _ <- cube ctx  change1

    pure unit
 

main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
  h <- height
  w <- width
  _ <-setCanvasHeight h canvas
  void $ setCanvasWidth w canvas
  void $ translate {translateX:w/2.0,translateY:h/2.0} ctx
  let p = P3d{x :(-500.0),y:(-500.0),z:500.0}
  addEventListener canvas "mouseup" onMouseUp
  addEventListener canvas "mousedown" onMouseDown
  addEventListener canvas "mousemove" onMouseMove
  animation ctx updateCube
  pure unit
  
