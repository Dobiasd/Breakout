module Breakout where

import Keyboard
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

breakoutBlue = rgb 60 60 100
textBlue = rgb 160 160 200
brickColorFactor = 0.01
wonTextHeight = 28
msgTextPosY = 20 - gameHeight/2
brickRows = 6
brickCols = 7


-- Inputs

type Input = { space:Bool, dir:Int, delta:Time }

delta = inSeconds <~ fps framesPerSecond

input = sampleOn delta (Input <~ Keyboard.space
                               ~ lift .x Keyboard.arrows
                               ~ delta)


-- Model

data State = Play | Serve | Won

type Positioned a = { a | x:Float, y:Float }
type Moving     a = { a | vx:Float, vy:Float }
type Sized      a = { a | w:Float, h:Float }

type Ball = Moving (Positioned { r:Float })
ball : Float -> Float -> Float -> Float -> Float -> Ball
ball x y vx vy r = {x=x, y=y, vx=vx, vy=vy, r=r }

type Player = Sized (Moving (Positioned {}))
player : Float -> Float -> Float -> Float -> Float -> Float -> Player
player x y vx vy w h = {x=x, y=y, vx=vx, vy=vy, w=w, h=h }

type Brick = Sized (Positioned {})
brick : Float -> Float -> Float -> Float -> Brick
brick x y w h = {x=x, y=y, w=w, h=h }

type Game = { state:State, gameBall:Ball, player:Player, bricks:[Brick] }

brickRow : Float -> [Brick]
brickRow y =
  let xOff = toFloat (ceiling  (-brickCols / 2)) * brickDistX
  in map (\x -> brick (brickDistX * x + xOff) y brickWidth brickHeight)
       [0..brickCols-1]

defaultGame : Game
defaultGame =
  { state    = Serve,
    gameBall = ball 0 (paddleYPos + ballRadius) 0 0 ballRadius,
    player   = player 0 paddleYPos 0 0 paddleWidths paddleHeight,
    bricks   = map ((*) brickDistY) [0..brickRows-1] |>
                 map brickRow |> concat }


-- Updates

stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx*t, y <- y + vy*t }

near k c n = n >= k-c && n <= k+c

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

weightedAvg : [Float] -> [Float] -> Float
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

gameState = foldp stepGame defaultGame input


-- Display

txt f = text . f . monospace . Text.color textBlue . toText
msg = "SPACE to serve, &larr; and &rarr; to move"
congrats = "Congratulations! SPACE to restart"
make color obj shape = shape |> filled color
                             |> move (obj.x,obj.y)
brickColor b = hsv (brickColorFactor * (b.x + b.y)) 1 1

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

main = lift2 display Window.dimensions <| dropRepeats gameState