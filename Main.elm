module Breakout where

{-| A simple clone of the classic arcade game Breakout.

Destroy all bricks with the ball, moving the paddle my mouse or touch.
The number of spare balls is restricted. Every lost ball and every
paddle touch cost points. Every hit brick adds points.

To add horizontal speed to the ball, move the paddle while serving/hitting.
-}

import Color(Color, rgb, rgba, hsl, lightGray, darkGray)
import Graphics.Element(Element, layers)
import Graphics.Collage(filled, move, Shape, Form, rect, group, traced, solid
  , circle, toForm, collage, scale)
import Keyboard
import List((::))
import List
import Maybe
import Signal((<~),(~))
import Signal
import String
import Text
import Time
import Touch
import Window


-- /---------------------\
-- | model configuration |
-- \---------------------/

{-| The game field extends from -300 to +300 in x coordinates
                       and from -200 to +200 in y coordinates. -}
(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (toFloat gameWidth / 2, toFloat gameHeight / 2)

{-| We aim for a maximally smooth gameplay. -}
framesPerSecond = 60

paddleSpeed = 110
paddleWidths = 52
paddleHeight = 4
brake = 0.7 -- If no input is present, the paddle will slow down.
traction = 0.55 -- How much does the paddle speed influence the ball speed?
serveSpeed = 200 -- initial y speed of the ball
speedIncX = 1.01 -- the ball speeds up during play
speedIncY = 1.02 -- in both directions
paddleYPos = 40 - gameHeight/2 -- paddle is near the screens bottom
brickDistX = 80
brickDistY = 33
brickWidth = 50
brickHeight = 10
ballRadius = 7
startSpareBalls = 2
brickRows = 6
brickCols = 7
pointsPerBrick = 100
pointsPerBall = -10
pointsPerContact = -1
speedFactor = 1


-- /--------------------\
-- | view configuration |
-- \--------------------/

manualMsg = "SPACE to serve, &larr; and &rarr; to move;" ++
            " or just touch the quadrants"
wonMsg = "Congratulations! Serve to restart."
lostMsg = "Serve to restart. ;)"
brickColorFactor = 0.01
endTextHeight = 24
msgTextPosY = 20 - halfHeight
pointsTextPos = (64 - halfWidth, halfHeight - 10)
spareBallsTextPos = (halfWidth - 69, halfHeight - 10)
breakoutBlue = rgb   60  60 100
textBlue     = rgb  160 160 200
quadrantCol  = rgba   0   0   0 0.4


-- /--------\
-- | inputs |
-- \--------/

{-| Check if the user touched one of the four screen quadrants. -}
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


maybe : b -> (a -> b) -> Maybe.Maybe a -> b
maybe def f val = Maybe.withDefault def (Maybe.map f val)

touchUpperRight : (Int,Int) -> Touch.Touch -> Bool
touchUpperRight = (<<) (maybe False identity) << touchInQuadrant 1

touchUpperLeft : (Int,Int) -> Touch.Touch -> Bool
touchUpperLeft = (<<) (maybe False identity) << touchInQuadrant 2

touchLowerLeft : (Int,Int) -> Touch.Touch -> Bool
touchLowerLeft = (<<) (maybe False identity) << touchInQuadrant 3

touchLowerRight : (Int,Int) -> Touch.Touch -> Bool
touchLowerRight = (<<) (maybe False identity) << touchInQuadrant 4

{-| Was the upper half of the screen touched? -}
touchUpper : (Int,Int) -> Touch.Touch -> Bool
touchUpper (w,h) t = touchUpperLeft (w,h) t || touchUpperRight (w,h) t

{-| Touching the upper quadrant can be used to serve like the space key. -}
spaceSignal : Signal.Signal Bool
spaceSignal =
  let
    f space touches (w,h) = space || List.any (touchUpper (w,h)) touches
  in
    Signal.map3 f Keyboard.space Touch.touches Window.dimensions

{-| The paddle can be moved with the arrow keys
or by touching the lower quadrants. -}
dirSignal : Signal.Signal Int
dirSignal =
  let
    f arrows touches (w,h) =
      let
        touchLeft = if List.any (touchLowerLeft (w,h)) touches then 1 else 0
        touchRight = if List.any (touchLowerRight (w,h)) touches then 1 else 0
      in
        arrows.x + touchRight - touchLeft
  in
    Signal.map3 f Keyboard.arrows Touch.touches Window.dimensions

{-| Game speed can be adjusted. -}
delta : Signal.Signal Float
delta = Signal.map (\d -> speedFactor * d)
        <| Time.inSeconds <~ Time.fps framesPerSecond

{-| Relevant things that can change are:
- did the user serve the ball?
- input direction of the paddle
- time delta between this and the last frame. -}
type alias Input = { space:Bool, dir:Int, delta:Time.Time }

input : Signal.Signal Input
input = Signal.sampleOn delta (Input <~ spaceSignal
                                      ~ dirSignal
                                      ~ delta)


-- /-------\
-- | model |
-- \-------/

type alias Positioned a = { a | x:Float, y:Float }
type alias Moving     a = { a | vx:Float, vy:Float }
type alias Sized      a = { a | w:Float, h:Float }

type alias Box = Sized (Positioned {})
type alias Brick = Box

type alias Ball = Moving (Positioned { r:Float })
type alias Player = Moving Box

type State = Play | Serve | Won | Lost

ball : Float -> Float -> Float -> Float -> Float -> Ball
ball x y vx vy r = {x=x, y=y, vx=vx, vy=vy, r=r }

player : Float -> Float -> Float -> Float -> Float -> Float -> Player
player x y vx vy w h = {x=x, y=y, vx=vx, vy=vy, w=w, h=h }

brick : Float -> Float -> Float -> Float -> Brick
brick x y w h = {x=x, y=y, w=w, h=h }

{-| Creation of one single row of bricks with equidistant gaps. -}
brickRow : Float -> List Brick
brickRow y =
  let xOff = toFloat (ceiling (-brickCols / 2)) * brickDistX
  in List.map (\x -> brick (brickDistX * x + xOff) y brickWidth brickHeight)
       [0..brickCols-1]

type alias Game = { state:State
                  , gameBall:Ball
                  , player:Player

-- The bricks still left in the game.
                  , bricks:List Brick

-- How many balls are left? (excluding the one currently played)
                  , spareBalls:Int

-- Count the number of contacts of the paddle with a ball.
                  , contacts:Int }

defaultGame : Game
defaultGame =
  { state      = Serve
  , gameBall   = ball 0 (paddleYPos + ballRadius) 0 0 ballRadius
  , player     = player 0 paddleYPos 0 0 paddleWidths paddleHeight
  , bricks     = List.map ((*) brickDistY) [0..brickRows-1] |>
                   List.map brickRow |> List.concat
  , spareBalls = startSpareBalls
  , contacts   = 0
  }


-- /---------\
-- | updates |
-- \---------/

{-| Move an object according to its speed for a given time step t. -}
stepObj : Float -> Moving (Positioned a) -> Moving (Positioned a)
stepObj t ({x,y,vx,vy} as obj) =
    { obj | x <- x + vx*t, y <- y + vy*t }

{-| Is the distance between n and k less or equal c? -}
near : number -> number -> number -> Bool
near k c n = n >= k-c && n <= k+c

{-| I the ball overlapping the box? -}
within : Ball -> Sized (Positioned a) -> Bool
within ball box = (ball.x |> near box.x (ball.r + box.w / 2))
               && (ball.y |> near box.y (ball.r + box.h / 2))

{-| Keep an object with speed v inside its lower and upper bounds. -}
stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
  if | lowerCollision -> abs v
     | upperCollision -> 0 - abs v
     | otherwise      -> v

{-| Increate the speed of a moving object. -}
speedUp : Moving a -> Moving a
speedUp ({vx, vy} as obj) = {obj | vx <- speedIncX * vx
                                 , vy <- speedIncY * vy }

{-| Simple weighted arithmetic mean. -}
weightedAvg : List number -> List number -> number
weightedAvg values weights =
  let
    weightedVals = List.map2 (*) values weights
  in
    List.sum weightedVals / List.sum weights

{-| foldr function for ball brick collisions. -}
goBrickHits : Brick -> (Ball,List Brick) -> (Ball,List Brick)
goBrickHits brick (ball,bricks) =
  let
    hit = ball `within` brick
    bricks' = if hit then bricks else brick::bricks
    ball' = if hit then { ball | vy <- -ball.vy } else ball
  in
    (if hit then speedUp ball' else ball', bricks')

{-| Collision handling of the ball with the paddle and the bricks
during a given timestep.
Returns the new ball properties, the bricks left and perhaps increased
count of paddle ball contact. -}
stepBall : Time.Time -> Ball -> Player -> List Brick -> Int
           -> ((Ball, List Brick), Int)
stepBall t ({x,y,vx,vy} as ball) p bricks contacts =
  let
    hitPlayer = (ball `within` p)
    contacts' = if hitPlayer then contacts + 1 else contacts
    newVx = if hitPlayer then
               weightedAvg [p.vx, vx] [traction, 1-traction] else
               stepV vx (x < (ball.r-halfWidth)) (x > halfWidth-ball.r)
    hitCeiling = (y > halfHeight - ball.r)
    ball' = stepObj t { ball | vx <- newVx ,
                               vy <- stepV vy hitPlayer hitCeiling }
  in
    (List.foldr goBrickHits (ball',[]) bricks, contacts')

{-| Calculate how the players properties have changed. -}
stepPlayer : Time.Time -> Int -> Player -> Player
stepPlayer t dir p =
  let p1 = stepObj t { p | vx <- p.vx * brake + toFloat dir * paddleSpeed }
  in  { p1 | x <- clamp (p.w/2-halfWidth) (halfWidth-p.w/2) p1.x }

{-| Update player position and
dispatch according to the current game state. -}
stepGame : Input -> Game -> Game
stepGame ({dir,delta} as input) ({state,player} as game) =
  let
    func = if | state == Play  -> stepPlay
              | state == Serve -> stepServe
              | otherwise      -> stepGameOver
  in
    func input { game | player <- stepPlayer delta dir player }

{-| Step game when the ball is bouncing around. -}
stepPlay : Input -> Game -> Game
stepPlay {delta} ({gameBall,player,bricks,spareBalls,contacts} as game) =
  let
    ballLost = gameBall.y - gameBall.r < -halfHeight
    gameOver = ballLost && spareBalls == 0
    spareBalls' = if ballLost then spareBalls - 1 else spareBalls
    state' = if | gameOver -> Lost
                | ballLost -> Serve
                | List.isEmpty bricks -> Won
                | otherwise -> Play
    ((ball', bricks'), contacts') =
      stepBall delta gameBall player bricks contacts
  in
    { game | state      <- state'
           , gameBall   <- ball'
           , bricks     <- bricks'
           , spareBalls <- max 0 spareBalls' -- No -1 when game is lost.
           , contacts   <- contacts' }

{-| Step game when the player needs to serve the ball. -}
stepServe : Input -> Game -> Game
stepServe {space} ({player,gameBall} as game) =
  let
    newBall = ball player.x (player.y + player.h/2 + gameBall.r + 1)
                   (traction*player.vx) serveSpeed gameBall.r
    state' = if space then Play else Serve
  in
    { game | state    <- state'
           , gameBall <- newBall }

{-| Change nothing except the user wants to play again. -}
stepGameOver : Input -> Game -> Game
stepGameOver {space} ({state} as game) = if space then defaultGame else game

gameState : Signal.Signal Game
gameState = Signal.foldp stepGame defaultGame input


-- /---------\
-- | display |
-- \---------/

{-| Render text using a given transformation function. -}
txt : (Text.Text -> Text.Text) -> String -> Element
txt f = Text.fromString
        >> Text.color textBlue
        >> Text.monospace
        >> f
        >> Text.leftAligned

{-| Take a shape, give it a color and move it to the objects position. -}
make : Color -> Positioned a -> Shape -> Form
make color obj shape = shape |> filled color
                             |> move (obj.x,obj.y)

{-| Generate the rainbow color of a brick depending on its position. -}
brickColor : Brick -> Color
brickColor b = hsl (brickColorFactor * (b.x + b.y)) 1 0.5

{-| Dummy for cases in which an game object or text should be invisible. -}
noForm : Form
noForm = rect 0 0 |> filled (rgba 0 0 0 0)

{-| Draw the touch screen quadrants required for controlling the game. -}
displayQuadrants : (Float,Float) -> State -> Form
displayQuadrants (w,h) state =
  let
    grid  = group
              [ [(0   ,0), (0  ,-h/2)] |> traced (solid quadrantCol)
              , [(-w/2,0), (w/2,   0)] |> traced (solid quadrantCol)
              ]
  in
    if state == Serve then grid else noForm

{-| How many points does the player have with his current game statistics,
and how many can he achieve maximally in a game? -}
calcPoints : Int -> Int -> Int -> (Int,Int)
calcPoints bricksLeft spareBalls contacts =
  let
    maxBricks = brickRows * brickCols
    maxPoints = pointsPerBrick * maxBricks
    bricksGone = maxBricks - bricksLeft
    points = pointsPerBrick * bricksGone +
             pointsPerBall * (startSpareBalls - spareBalls) +
             pointsPerContact * contacts
  in
    (points,maxPoints)

{-| Generate the String showing the number of curent points. -}
pointsText : Int -> Int -> Int -> String
pointsText bricksLeft spareBalls contacts =
  let
    (points,maxPoints) = calcPoints bricksLeft spareBalls contacts
    maxPointsStrLen = String.length <| toString maxPoints
  in
    "points: " ++ (String.padLeft maxPointsStrLen ' ' <| toString points)

{-| Draw background into a fullscreen-sized Element. -}
displayBricks : (Int,Int) -> List Brick -> Element
displayBricks (w, h) bricks =
  let
    background = rect gameWidth gameHeight |> filled breakoutBlue
    brickRects = group <| List.map (\b -> rect b.w b.h |> make (brickColor b) b)
                                   bricks
    gameScale = min (toFloat w / gameWidth) (toFloat h / gameHeight)
  in
    displayFullScreen (w, h) (group [ background, brickRects ])

{-| Draw foreground into a fullscreen-sized Element. -}
displayForeground : (Int,Int) -> Game -> Element
displayForeground (w,h) {state,gameBall,player,bricks,spareBalls,contacts} =
  let
    pointsMsg = pointsText (List.length bricks) spareBalls contacts
    spareBallsMsg = "spare balls: " ++ toString spareBalls
    ball = circle gameBall.r |> make lightGray gameBall
    paddle = rect player.w player.h |> make darkGray player
    serveTextForm = if state == Serve then txt identity manualMsg |> toForm
                            |> move (0, msgTextPosY)
                            else noForm
    endMsg = case state of
               Won -> wonMsg
               Lost -> lostMsg
               _ -> ""
    showEndText = state == Won || state == Lost
    endText = txt (Text.height endTextHeight) (pointsMsg ++ "\n" ++ endMsg)
    endTextForm = if showEndText then endText |> toForm else noForm
    quadrants = displayQuadrants (gameWidth,gameHeight) state
    pointsTextForm = txt identity pointsMsg |> toForm |> move pointsTextPos
    spareBallsForm = txt identity spareBallsMsg |> toForm
      |> move spareBallsTextPos
  in
    displayFullScreen (w, h) <|
      group
        [ paddle
        , ball
        , serveTextForm
        , pointsTextForm
        , spareBallsForm
        , endTextForm
        , quadrants
        ]

{-| Draw a Form maximized into the window. -}
displayFullScreen : (Int,Int) -> Form -> Element
displayFullScreen (w,h) content =
  let
    gameScale = min (toFloat w / gameWidth) (toFloat h / gameHeight)
  in
    collage w h [content |> scale gameScale]

{-| Change in brick configuration. -}
bricksSignal : Signal.Signal (List Brick)
bricksSignal = (.bricks <~ gameState) |> Signal.dropRepeats

{-| The changing background Element. -}
background : Signal.Signal Element
background = displayBricks <~ Window.dimensions ~ bricksSignal

{-| The changing foreground Element. -}
foreground : Signal.Signal Element
foreground = displayForeground <~ Window.dimensions ~ gameState

{-| Display the game.
Background and foreground are splitted, so the background
is only redrawn when really needed, i.e. bricks change. -}
display : Element -> Element -> Element
display background foreground = layers [background, foreground]

main = display <~ background ~ foreground