module Breakout where

import Keyboard
import String
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
startSpareBalls = 2
brickRows = 6
brickCols = 7
pointsPerBrick = 1000

-- view configuration

manualMsg = "SPACE to serve, &larr; and &rarr; to move; or just touch the quadrants"
wonMsg = "Congratulations! Serve to restart."
lostMsg = "No luck this time. Serve to restart. ;)"
breakoutBlue = rgb 60 60 100
textBlue = rgb 160 160 200
brickColorFactor = 0.01
endTextHeight = 24
msgTextPosY = 20 - halfHeight
pointsTextPos = (64 - halfWidth, halfHeight - 10)
spareBallsTextPos = (halfWidth - 69, halfHeight - 10)
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
                              _ -> (False, (==), (==))
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

data State = Play | Serve | Won | Lost

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

type Game = { state:State
            , gameBall:Ball
            , player:Player
            , bricks:[Brick]
            , spareBalls:Int
            }

defaultGame : Game
defaultGame =
  { state      = Serve
  , gameBall   = ball 0 (paddleYPos + ballRadius) 0 0 ballRadius
  , player     = player 0 paddleYPos 0 0 paddleWidths paddleHeight
  , bricks     = map ((*) brickDistY) [0..brickRows-1] |>
                   map brickRow |> concat
  , spareBalls = startSpareBalls
  }


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
               stepV vx (x < (ball.r-halfWidth)) (x > halfWidth-ball.r)
    hitCeiling = (y > halfHeight - ball.r)
    ball1 = stepObj t { ball | vx <- newVx ,
                               vy <- stepV vy hitPlayer hitCeiling }
  in
    foldr goBrickHits (ball1,[]) bricks


stepPlyr : Time -> Int -> Player -> Player
stepPlyr t dir p =
  let p1 = stepObj t { p | vx <- p.vx * brake + toFloat dir * paddleSpeed }
  in  { p1 | x <- clamp (p.w/2-halfWidth) (halfWidth-p.w/2) p1.x }

nextState : Bool -> Game -> (State, Int)
nextState space ({state,gameBall,bricks,spareBalls} as game) =
  let
    ballLost = state == Play && gameBall.y - gameBall.r < -halfHeight
    spareBalls' = if ballLost then spareBalls - 1 else spareBalls
    gameOver = spareBalls' == -1 && ballLost && state /= Won
    state' =  if | state == Serve && space -> Play
                 | gameOver -> Lost
                 | state == Play && ballLost -> Serve
                 | isEmpty bricks -> Won
                 | otherwise -> state

  in
    (state', max 0 spareBalls')

stepGame : Input -> Game -> Game
stepGame {space,dir,delta}
         ({state,gameBall,player,bricks,spareBalls} as game) =
  let
    newBall = ball player.x (player.y + player.h/2 + gameBall.r)
                   (traction*player.vx) serveSpeed gameBall.r
    ballLost = state == Play && gameBall.y < -halfHeight
    (state', spareBalls') = nextState space game
    (ball', bricks') = case state of
                         Serve -> (newBall, bricks)
                         Lost -> (gameBall, bricks)
                         _ -> (stepBall delta gameBall player bricks)
  in
    if (state' == Won || state' == Lost) && space then defaultGame else
      { game | state      <- state'
             , gameBall   <- ball'
             , player     <- stepPlyr delta dir player
             , bricks     <- bricks'
             , spareBalls <- spareBalls' }

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

displayQuadrants : (Float,Float) -> State -> Form
displayQuadrants (w,h) state =
  let
    grid  = group
              [ [(0   ,0), (0  ,-h/2)] |> traced (solid quadrantCol)
              , [(-w/2,0), (w/2,   0)] |> traced (solid quadrantCol)
              ]
  in
    if state == Serve then grid else spacer 0 0 |> toForm

pointsText : Int -> String
pointsText bricksLeft =
  let
    maxBricks = brickRows * brickCols
    maxPoints = pointsPerBrick * maxBricks
    points = maxPoints - pointsPerBrick * bricksLeft
    maxPointsStrLen = String.length <| show maxPoints
  in
    "points: " ++ (String.padLeft maxPointsStrLen ' ' <| show points)

display : (Int,Int) -> Game -> Element
display (w,h) {state,gameBall,player,bricks,spareBalls} =
  let
    pointsMsg = pointsText <| length bricks
    spareBallsMsg = "spare balls: " ++ show spareBalls
    noElem = spacer 0 0
    background = rect gameWidth gameHeight |> filled breakoutBlue
    ball = circle gameBall.r |> make lightGray gameBall
    paddle = rect player.w player.h |> make darkGray player
    serveTextForm = toForm (if state == Serve then txt id manualMsg
                            else noElem) |> move (0, msgTextPosY)
    endMsg = case state of
               Won -> wonMsg
               Lost -> lostMsg
               _ -> ""
    showEndText = state == Won || state == Lost
    endText = txt (Text.height endTextHeight) (pointsMsg ++ "\n" ++ endMsg)
    endTextForm = (if showEndText then endText else noElem) |> toForm
    brickRects = group <| map (\b -> rect b.w b.h |> make (brickColor b) b)
                            bricks
    quadrants = displayQuadrants (gameWidth,gameHeight) state
    pointsTextForm = txt id pointsMsg |> toForm |> move pointsTextPos
    spareBallsForm = txt id spareBallsMsg |> toForm |> move spareBallsTextPos
  in
    container w h middle <| collage gameWidth gameHeight <|
      [ background
      , brickRects
      , paddle
      , ball
      , serveTextForm
      , pointsTextForm
      , spareBallsForm
      , endTextForm
      , quadrants
      ]

main = lift2 display Window.dimensions <| dropRepeats gameState