module Breakout where

import Keyboard
import Touch
import Window


-- model configuration

framesPerSecond = 60
paddleSpeed = 110
paddleWidths = 52
paddleHeight = 4
brake = 0.7
traction = 0.7
serveSpeed = 200
speedIncX = 1.01
speedIncY = 1.02
paddleYPos = 40 - gameHeight/2
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (toFloat gameWidth / 2, toFloat gameHeight / 2)
brickDistX = 80
brickDistY = 33
brickWidth = 50
brickHeight = 10
ballRadius = 7


-- view configuration

msg = "SPACE to serve, &larr; and &rarr; to move; or just touch the quadrants"
congrats = "Congratulations! Serve to restart."
breakoutBlue = rgb 60 60 100
textBlue = rgb 160 160 200
brickColorFactor = 0.01
wonTextHeight = 28
msgTextPosY = 20 - gameHeight/2
brickRows = 6
brickCols = 7
quadrantCol = rgba 0 0 0 0.4


-- Inputs

touchInQuadrant : Int -> (Int,Int) -> Touch.Touch -> Maybe Bool
touchInQuadrant q (w,h) touch =
  let
    (centerX,centerY) = (toFloat w / 2, toFloat h / 2)
    (x,y) = (toFloat touch.x, toFloat touch.y)
    (qExists, xCmp, yCmp) = case q of
                              1 -> (True, (>), (<))
                              2 -> (True, (<), (<))
                              3 -> (True, (<), (>))
                              4 -> (True, (>), (>))
                              otherwise -> (False, (==), (==))
  in
    if qExists then Just (x `xCmp` centerX && y `yCmp` centerY) else Nothing

touchUpperRight : (Int,Int) -> Touch.Touch -> Bool
touchUpperRight = (.) (maybe False id) . touchInQuadrant 1

touchUpperLeft : (Int,Int) -> Touch.Touch -> Bool
touchUpperLeft = (.) (maybe False id) . touchInQuadrant 2

touchLowerLeft : (Int,Int) -> Touch.Touch -> Bool
touchLowerLeft = (.) (maybe False id) . touchInQuadrant 3

touchLowerRight : (Int,Int) -> Touch.Touch -> Bool
touchLowerRight = (.) (maybe False id) . touchInQuadrant 4

touchUpper : (Int,Int) -> Touch.Touch -> Bool
touchUpper (w,h) t = touchUpperLeft (w,h) t || touchUpperRight (w,h) t

spaceSignal : Signal Bool
spaceSignal =
  let
    f space touches (w,h) = space || any (touchUpper (w,h)) touches
  in
    lift3 f Keyboard.space Touch.touches Window.dimensions

dirSignal : Signal Int
dirSignal =
  let
    f arrows touches (w,h) =
      let
        touchLeft = if any (touchLowerLeft (w,h)) touches then 1 else 0
        touchRight = if any (touchLowerRight (w,h)) touches then 1 else 0
      in
        arrows.x + touchRight - touchLeft
  in
    lift3 f Keyboard.arrows Touch.touches Window.dimensions

type Input = { space:Bool, dir:Int, delta:Time }

delta : Signal Float
delta = inSeconds <~ fps framesPerSecond

input : Signal Input
input = sampleOn delta (Input <~ spaceSignal
                               ~ dirSignal
                               ~ delta)


-- Model

data State = Play | Serve | Won

type Positioned a = { a | x:Float, y:Float }
type Moving     a = { a | vx:Float, vy:Float }
type Sized      a = { a | w:Float, h:Float }

type Box = Sized (Positioned {})

type Brick = Box
type Ball = Moving (Positioned { r:Float })
type Player = Sized (Moving (Positioned {}))

ball : Float -> Float -> Float -> Float -> Float -> Ball
ball x y vx vy r = {x=x, y=y, vx=vx, vy=vy, r=r }

player : Float -> Float -> Float -> Float -> Float -> Float -> Player
player x y vx vy w h = {x=x, y=y, vx=vx, vy=vy, w=w, h=h }

brick : Float -> Float -> Float -> Float -> Brick
brick x y w h = {x=x, y=y, w=w, h=h }

brickRow : Float -> [Brick]
brickRow y =
  let xOff = toFloat (ceiling  (-brickCols / 2)) * brickDistX
  in map (\x -> brick (brickDistX * x + xOff) y brickWidth brickHeight)
       [0..brickCols-1]

type Game = { state:State, gameBall:Ball, player:Player, bricks:[Brick] }

defaultGame : Game
defaultGame =
  { state    = Serve,
    gameBall = ball 0 (paddleYPos + ballRadius) 0 0 ballRadius,
    player   = player 0 paddleYPos 0 0 paddleWidths paddleHeight,
    bricks   = map ((*) brickDistY) [0..brickRows-1] |>
                 map brickRow |> concat }


-- Updates

stepObj : Float -> Moving (Positioned a) -> Moving (Positioned a)
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx*t, y <- y + vy*t }

near : number -> number -> number -> Bool
near k c n = n >= k-c && n <= k+c

within : Ball -> Sized (Positioned a) -> Bool
within ball box = (ball.x |> near box.x (ball.r + box.w / 2))
               && (ball.y |> near box.y (ball.r + box.h / 2))

stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

speedUp : Ball -> Ball
speedUp ({vx, vy} as ball) = {ball | vx <- speedIncX * vx
                                   , vy <- speedIncY * vy }

goBrickHits : Brick -> (Ball,[Brick]) -> (Ball,[Brick])
goBrickHits brick (ball,bricks) =
  let
    hit = ball `within` brick
    bricks' = if hit then bricks else brick::bricks
    ball' = if hit then { ball | vy <- -ball.vy } else ball
  in
    (if hit then speedUp ball' else ball', bricks')

weightedAvg : [number] -> [number] -> number
weightedAvg values weights =
  let
    weightedVals = zipWith (*) values weights
  in
    sum weightedVals / sum weights

stepBall : Time -> Ball -> Player -> [Brick] -> (Ball,[Brick])
stepBall t ({x,y,vx,vy} as ball) p bricks =
  let
    hitPlayer = (ball `within` p)
    newVx = if hitPlayer then
               weightedAvg [p.vx, vx] [traction, 1-traction] else
               stepV vx (x < ball.r-halfWidth) (x > halfWidth-ball.r)
    hitCeiling = (y > halfHeight-ball.r)
    ball1 = stepObj t { ball | vx <- newVx ,
                               vy <- stepV vy hitPlayer hitCeiling }
  in
    foldr goBrickHits (ball1,[]) bricks


stepPlyr : Time -> Int -> Player -> Player
stepPlyr t dir p =
  let p1 = stepObj t { p | vx <- p.vx * brake + toFloat dir * paddleSpeed }
  in  { p1 | x <- clamp (p.w/2-halfWidth) (halfWidth-p.w/2) p1.x
                , vx <- if abs p.vx < 1 then p1.vx else p1.vx }

stepGame : Input -> Game -> Game
stepGame {space,dir,delta} ({state,gameBall,player,bricks} as game) =
  let
    newBall = ball player.x (player.y + player.h/2 + gameBall.r)
                   (traction*player.vx) serveSpeed gameBall.r
    ballLost = gameBall.y < -halfHeight
    (ball', bricks') = if | state == Serve -> (newBall, bricks)
                          | otherwise -> stepBall delta gameBall player bricks
    nextState = if | state == Serve && space -> Play
                   | state == Play && ballLost -> Serve
                   | isEmpty bricks -> Won
                   | otherwise -> state
  in
    if state == Won && space then defaultGame else
      { game | state    <- nextState
             , gameBall <- ball'
             , player   <- stepPlyr delta dir player
             , bricks   <- bricks' }

gameState : Signal Game
gameState = foldp stepGame defaultGame input


-- Display
txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color textBlue . toText

make : Color -> Positioned a -> Shape -> Form
make color obj shape = shape |> filled color
                             |> move (obj.x,obj.y)

brickColor : Brick -> Color
brickColor b = hsv (brickColorFactor * (b.x + b.y)) 1 1

displayQuadrants : (Float,Float) -> Form
displayQuadrants (w,h) =
    group
      [ [(0   ,0), (0  ,-h/2)] |> traced (solid quadrantCol)
      , [(-w/2,0), (w/2,   0)] |> traced (solid quadrantCol)
      ]

display : (Int,Int) -> Game -> Element
display (w,h) {state,gameBall,player,bricks} =
  container w h middle <| collage gameWidth gameHeight <|
    [ rect gameWidth gameHeight |> filled breakoutBlue
    , circle gameBall.r |> make lightGray gameBall
    , rect player.w player.h |> make darkGray player
    , toForm (if state == Serve then txt id msg else spacer 1 1)
        |> move (0, msgTextPosY)
    , toForm (if state == Won then txt (Text.height wonTextHeight)
        congrats else spacer 1 1)
    ] ++ map (\b -> rect b.w b.h |> make (brickColor b) b) bricks
      ++ if state == Serve then [displayQuadrants (toFloat w, toFloat h)]
                           else []

main = lift2 display Window.dimensions <| dropRepeats gameState