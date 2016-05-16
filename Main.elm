module Main exposing (..)

import Html exposing (Html, Attribute, div, text, input)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck)
import Color exposing (Color, rgb, rgba, hsl, lightGray, darkGray)
import Element exposing (Element, layers, leftAligned)
import Collage
    exposing
        ( filled
        , move
        , Shape
        , Form
        , rect
        , group
        , traced
        , solid
        , circle
        , toForm
        , collage
        , scale
        , path
        )
import Json.Decode as D exposing ((:=))
import Keyboard
import List exposing ((::))
import List
import Maybe
import String
import Task
import Text
import Time
import Window
import AnimationFrame
import Update.Extra.Infix exposing ((:>))


main =
    program
        { init = initModelAndCommands
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


initModelAndCommands : ( Model, Cmd Msg )
initModelAndCommands =
    ( defaultModel, getWindowSizeCommand )


getWindowSizeCommand : Cmd Msg
getWindowSizeCommand =
    Task.perform (always NoOp) WindowSize Window.size



-- MODEL


modelCfg =
    { -- The game field extends from -300 to +300 in x coordinates
      --                    and from -200 to +200 in y coordinates.
      gameWidth = 600
    , gameHeight = 400
    , halfWidth = 300.0
    , halfHeight = 200.0
    , paddleSpeed = 110
    , paddleWidths = 52
    , paddleHeight = 4
    , brake =
        0.7
        -- If no input is present, the paddle will slow down.
    , traction =
        0.55
        -- How much does the paddle speed influence the ball speed?
    , serveSpeed =
        200
        -- initial y speed of the ball
    , speedIncX =
        1.01
        -- the ball speeds up during play
    , speedIncY =
        1.02
        -- in both directions
    , paddleYPos =
        -160
        -- paddle is near the screens bottom
    , brickDistX = 80
    , brickDistY = 33
    , brickWidth = 50
    , brickHeight = 10
    , ballRadius = 7
    , startSpareBalls = 2
    , brickRows = 6
    , brickCols = 7
    , pointsPerBrick = 100
    , pointsPerBall = -10
    , pointsPerContact = -1
    , speedFactor = 1
    }


type alias Positioned a =
    { a | x : Float, y : Float }


type alias Moving a =
    { a | vx : Float, vy : Float }


type alias Sized a =
    { a | w : Float, h : Float }


type alias Box =
    Sized (Positioned {})


type alias Brick =
    Box


type alias Ball =
    Moving (Positioned { r : Float })


type alias Player =
    Moving (Sized (Positioned {}))


type State
    = Play
    | Serve
    | Won
    | Lost


ball : Float -> Float -> Float -> Float -> Float -> Ball
ball x y vx vy r =
    { x = x, y = y, vx = vx, vy = vy, r = r }


player : Float -> Float -> Float -> Float -> Float -> Float -> Player
player x y vx vy w h =
    { x = x, y = y, vx = vx, vy = vy, w = w, h = h }


brick : Float -> Float -> Float -> Float -> Brick
brick x y w h =
    { x = x, y = y, w = w, h = h }


{-| Creation of one single row of bricks with equidistant gaps.
-}
brickRow : Float -> List Brick
brickRow y =
    let
        xOff =
            toFloat (-modelCfg.brickCols // 2 |> toFloat |> ceiling)
                * modelCfg.brickDistX
    in
        List.map
            (\x ->
                brick (modelCfg.brickDistX * x + xOff)
                    y
                    modelCfg.brickWidth
                    modelCfg.brickHeight
            )
            ([0..modelCfg.brickCols - 1] |> List.map toFloat)


type alias Model =
    { state : State
    , gameBall : Ball
    , player : Player
    , bricks : List Brick
    , spareBalls : Int
    , contacts : Int
    , leftPressed : Bool
    , rightPressed : Bool
    , windowDimensions : Window.Size
    }


defaultModel : Model
defaultModel =
    { state = Serve
    , gameBall =
        ball 0
            (modelCfg.paddleYPos + modelCfg.ballRadius)
            0
            0
            modelCfg.ballRadius
    , player =
        player 0
            modelCfg.paddleYPos
            0
            0
            modelCfg.paddleWidths
            modelCfg.paddleHeight
    , bricks =
        List.map ((*) modelCfg.brickDistY)
            ([0..modelCfg.brickRows - 1] |> List.map toFloat)
            |> List.map brickRow
            |> List.concat
    , spareBalls = modelCfg.startSpareBalls
    , contacts = 0
    , leftPressed = False
    , rightPressed = False
    , windowDimensions = { width = 640, height = 480 }
    }



-- UPDATE


type Msg
    = NoOp
    | SpaceDown
    | LeftDown
    | LeftUp
    | RightDown
    | RightUp
    | TouchStart Float Float
    | TouchRelease
    | WindowSize Window.Size
    | Tick Time.Time


keyDownToMsg : Keyboard.KeyCode -> Msg
keyDownToMsg kc =
    case kc of
        32 ->
            SpaceDown

        37 ->
            LeftDown

        39 ->
            RightDown

        _ ->
            NoOp


keyUpToMsg : Keyboard.KeyCode -> Msg
keyUpToMsg kc =
    case kc of
        37 ->
            LeftUp

        39 ->
            RightUp

        _ ->
            NoOp


type alias TouchPosition =
    { x : Int
    , y : Int
    }


{-| Check if the user touched one of the four screen quadrants.
-}
touchInQuadrant : Int -> Window.Size -> TouchPosition -> Maybe Bool
touchInQuadrant q { width, height } touch =
    let
        ( centerX, centerY ) =
            ( toFloat width / 2, toFloat height / 2 )

        ( x, y ) =
            ( toFloat touch.x, toFloat touch.y )

        ( qExists, xCmp, yCmp ) =
            case q of
                1 ->
                    ( True, (>), (<) )

                2 ->
                    ( True, (<), (<) )

                3 ->
                    ( True, (<), (>) )

                4 ->
                    ( True, (>), (>) )

                _ ->
                    ( False, (==), (==) )
    in
        if qExists then
            Just (x `xCmp` centerX && y `yCmp` centerY)
        else
            Nothing


maybe : b -> (a -> b) -> Maybe.Maybe a -> b
maybe def f val =
    Maybe.withDefault def (Maybe.map f val)


touchUpperRight : Window.Size -> TouchPosition -> Bool
touchUpperRight =
    (<<) (maybe False identity) << touchInQuadrant 1


touchUpperLeft : Window.Size -> TouchPosition -> Bool
touchUpperLeft =
    (<<) (maybe False identity) << touchInQuadrant 2


touchLowerLeft : Window.Size -> TouchPosition -> Bool
touchLowerLeft =
    (<<) (maybe False identity) << touchInQuadrant 3


touchLowerRight : Window.Size -> TouchPosition -> Bool
touchLowerRight =
    (<<) (maybe False identity) << touchInQuadrant 4


{-| Was the upper half of the screen touched?
-}
touchUpper : Window.Size -> TouchPosition -> Bool
touchUpper winSize t =
    touchUpperLeft winSize t || touchUpperRight winSize t


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs keyDownToMsg
        , Keyboard.ups keyUpToMsg
        , AnimationFrame.diffs (\dt -> Tick (dt / 1000))
        , Window.resizes WindowSize
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newModel, cmds ) =
            case msg of
                NoOp ->
                    ( model, Cmd.none )

                SpaceDown ->
                    case model.state of
                        Serve ->
                            ( serveBall model, Cmd.none )

                        Won ->
                            initModelAndCommands

                        Lost ->
                            initModelAndCommands

                        Play ->
                            ( model, Cmd.none )

                LeftDown ->
                    ( { model | leftPressed = True }, Cmd.none )

                LeftUp ->
                    ( { model | leftPressed = False }, Cmd.none )

                RightDown ->
                    ( { model | rightPressed = True }, Cmd.none )

                RightUp ->
                    ( { model | rightPressed = False }, Cmd.none )

                WindowSize newSize ->
                    ( { model | windowDimensions = newSize }, Cmd.none )

                TouchStart posX posY ->
                    let
                        pos =
                            { x = round posX, y = round posY }
                    in
                        if touchUpper model.windowDimensions pos then
                            update SpaceDown model
                        else if touchLowerLeft model.windowDimensions pos then
                            update LeftDown model
                        else if touchLowerRight model.windowDimensions pos then
                            update RightDown model
                        else
                            ( model, Cmd.none )

                TouchRelease ->
                    ( model, Cmd.none ) :> update LeftUp :> update RightUp

                Tick dt ->
                    let
                        paddleDirection =
                            case ( model.leftPressed, model.rightPressed ) of
                                ( True, True ) ->
                                    0

                                ( False, True ) ->
                                    1

                                ( True, False ) ->
                                    -1

                                ( False, False ) ->
                                    0

                        model' =
                            { model
                                | player =
                                    stepPlayer dt
                                        paddleDirection
                                        model.player
                            }
                    in
                        case model.state of
                            Play ->
                                ( stepPlay dt model', Cmd.none )

                            Won ->
                                ( model', Cmd.none )

                            Lost ->
                                ( model', Cmd.none )

                            Serve ->
                                ( moveBallWithPaddle model', Cmd.none )
    in
        ( newModel, cmds )


moveBallWithPaddle : Model -> Model
moveBallWithPaddle ({ gameBall, player } as game) =
    let
        newBall =
            ball player.x
                (player.y + player.h / 2 + gameBall.r + 1)
                (modelCfg.traction * player.vx)
                modelCfg.serveSpeed
                gameBall.r
    in
        { game
            | gameBall = newBall
        }


{-| Move an object according to its speed for a given time step t.
-}
stepObj : Float -> Moving (Positioned a) -> Moving (Positioned a)
stepObj t ({ x, y, vx, vy } as obj) =
    { obj | x = x + vx * t, y = y + vy * t }


{-| Is the distance between n and k less or equal c?
-}
near : number -> number -> number -> Bool
near k c n =
    n >= k - c && n <= k + c


{-| Is the ball overlapping the box?
-}
within : Ball -> Sized (Positioned a) -> Bool
within ball box =
    (ball.x |> near box.x (ball.r + box.w / 2))
        && (ball.y |> near box.y (ball.r + box.h / 2))


{-| Keep an object with speed v inside its lower and upper bounds.
-}
stepV : Float -> Bool -> Bool -> Float
stepV v lowerCollision upperCollision =
    if lowerCollision then
        abs v
    else if upperCollision then
        0 - abs v
    else
        v


{-| Increate the speed of a moving object.
-}
speedUp : Moving a -> Moving a
speedUp ({ vx, vy } as obj) =
    { obj
        | vx = modelCfg.speedIncX * vx
        , vy = modelCfg.speedIncY * vy
    }


{-| Simple weighted arithmetic mean.
-}
weightedAvg : List Float -> List Float -> Float
weightedAvg values weights =
    let
        weightedVals =
            List.map2 (*) values weights
    in
        List.sum weightedVals / List.sum weights


{-| foldr function for ball brick collisions.
-}
goBrickHits : Brick -> ( Ball, List Brick ) -> ( Ball, List Brick )
goBrickHits brick ( ball, bricks ) =
    let
        hit =
            ball `within` brick

        bricks' =
            if hit then
                bricks
            else
                brick :: bricks

        ball' =
            if hit then
                speedUp { ball | vy = -ball.vy }
            else
                ball
    in
        ( ball', bricks' )


{-| Collision handling of the ball with the paddle and the bricks
during a given timestep.
Returns the new ball properties, the bricks left and perhaps increased
count of paddle ball contact.
-}
stepBall :
    Time.Time
    -> Ball
    -> Player
    -> List Brick
    -> Int
    -> ( ( Ball, List Brick ), Int )
stepBall t ({ x, y, vx, vy } as ball) p bricks contacts =
    let
        hitPlayer =
            (ball `within` p)

        contacts' =
            if hitPlayer then
                contacts + 1
            else
                contacts

        newVx =
            if hitPlayer then
                weightedAvg [ p.vx, vx ]
                    [ modelCfg.traction, 1 - modelCfg.traction ]
            else
                stepV vx
                    (x < (ball.r - modelCfg.halfWidth))
                    (x > modelCfg.halfWidth - ball.r)

        hitCeiling =
            (y > modelCfg.halfHeight - ball.r)

        ball' =
            stepObj t
                { ball
                    | vx = newVx
                    , vy = stepV vy hitPlayer hitCeiling
                }
    in
        ( List.foldr goBrickHits ( ball', [] ) bricks, contacts' )


{-| Calculate how the players properties have changed.
-}
stepPlayer : Time.Time -> Int -> Player -> Player
stepPlayer t dir p =
    let
        p1 =
            stepObj t
                { p
                    | vx =
                        p.vx
                            * modelCfg.brake
                            + toFloat dir
                            * modelCfg.paddleSpeed
                }
    in
        { p1
            | x =
                clamp (p.w / 2 - modelCfg.halfWidth)
                    (modelCfg.halfWidth - p.w / 2)
                    p1.x
        }


{-| Step game when the ball is bouncing around.
-}
stepPlay : Time.Time -> Model -> Model
stepPlay delta ({ gameBall, player, bricks, spareBalls, contacts } as model) =
    let
        ballLost =
            gameBall.y - gameBall.r < -modelCfg.halfHeight

        gameOver =
            ballLost && spareBalls == 0

        spareBalls' =
            if ballLost then
                spareBalls - 1
            else
                spareBalls

        state' =
            if gameOver then
                Lost
            else if ballLost then
                Serve
            else if List.isEmpty bricks then
                Won
            else
                Play

        ( ( ball', bricks' ), contacts' ) =
            stepBall delta gameBall player bricks contacts
    in
        { model
            | state = state'
            , gameBall = ball'
            , bricks = bricks'
            , spareBalls =
                Basics.max 0 spareBalls'
                -- No -1 when game is lost.
            , contacts = contacts'
        }


{-| Step game when the player needs to serve the ball.
-}
serveBall : Model -> Model
serveBall ({ player, gameBall } as model) =
    { model
        | state = Play
    }
        |> moveBallWithPaddle



-- VIEW


viewCfg =
    { manualMsg =
        "SPACE to serve, &larr; and &rarr; to move;"
            ++ " or just touch the quadrants"
    , wonMsg = "Congratulations! SPACE to restart."
    , lostMsg = "SPACE to restart. ;)"
    , brickColorFactor = 0.01
    , endTextHeight = 24
    , msgTextPosY = 20 - modelCfg.halfHeight
    , pointsTextPos = ( 64 - modelCfg.halfWidth, modelCfg.halfHeight - 10 )
    , spareBallsTxtPos = ( modelCfg.halfWidth - 69, modelCfg.halfHeight - 10 )
    , breakoutBlue = rgb 60 60 100
    , textBlue = rgb 160 160 200
    , quadrantCol = rgba 0 0 0 0.4
    }


touchDecoder : D.Decoder Msg
touchDecoder =
    D.oneOf
        [ D.at [ "touches", "0" ] (D.object2 TouchStart ("pageX" := D.float) ("pageY" := D.float))
        , D.object2 TouchStart ("pageX" := D.float) ("pageY" := D.float)
        ]


view : Model -> Html Msg
view model =
    div
        [ Html.Events.on "touchstart" touchDecoder
        , Html.Events.on "mousedown" touchDecoder
        , Html.Events.on "touchend" (D.succeed TouchRelease)
        , Html.Events.on "mouseup" (D.succeed TouchRelease)
        ]
        [ layers [ displayBricks model, displayForeground model ]
            |> Element.toHtml
        ]


displayFullScreen : Window.Size -> Form -> Element
displayFullScreen { width, height } content =
    let
        -- to prevent scrolling down when user hits space bar
        height' =
            height - 20

        gameScale =
            Basics.min (toFloat width / modelCfg.gameWidth)
                (toFloat height' / modelCfg.gameHeight)
    in
        collage width height' [ content |> scale gameScale ]


{-| Render text using a given transformation function.
-}
txt : (Text.Text -> Text.Text) -> String -> Element
txt f =
    Text.fromString
        >> Text.color viewCfg.textBlue
        >> Text.monospace
        >> f
        >> leftAligned


{-| Take a shape, give it a color and move it to the objects position.
-}
make : Color -> Positioned a -> Shape -> Form
make color obj shape =
    shape
        |> filled color
        |> move ( obj.x, obj.y )


{-| Generate the rainbow color of a brick depending on its position.
-}
brickColor : Brick -> Color
brickColor b =
    hsl (viewCfg.brickColorFactor * (b.x + b.y)) 1 0.5


{-| Dummy for cases in which an game object or text should be invisible.
-}
noForm : Form
noForm =
    rect 0 0 |> filled (rgba 0 0 0 0)


{-| How many points does the player have with his current game statistics,
and how many can he achieve maximally in a game?
-}
calcPoints : Int -> Int -> Int -> ( Int, Int )
calcPoints bricksLeft spareBalls contacts =
    let
        maxBricks =
            modelCfg.brickRows * modelCfg.brickCols

        maxPoints =
            modelCfg.pointsPerBrick * maxBricks

        bricksGone =
            maxBricks - bricksLeft

        points =
            modelCfg.pointsPerBrick
                * bricksGone
                + modelCfg.pointsPerBall
                * (modelCfg.startSpareBalls - spareBalls)
                + modelCfg.pointsPerContact
                * contacts
    in
        ( points, maxPoints )


{-| Generate the String showing the number of curent points.
-}
pointsText : Int -> Int -> Int -> String
pointsText bricksLeft spareBalls contacts =
    let
        ( points, maxPoints ) =
            calcPoints bricksLeft spareBalls contacts

        maxPointsStrLen =
            String.length <| toString maxPoints
    in
        "points: " ++ (String.padLeft maxPointsStrLen ' ' <| toString points)


displayBricks : Model -> Element
displayBricks ({ bricks } as model) =
    let
        background =
            rect modelCfg.gameWidth modelCfg.gameHeight
                |> filled viewCfg.breakoutBlue

        brickRects =
            group
                <| List.map (\b -> rect b.w b.h |> make (brickColor b) b)
                    bricks
    in
        displayFullScreen model.windowDimensions
            (group [ background, brickRects ])


{-| Draw the touch screen quadrants required for controlling the game.
-}
displayQuadrants : ( Float, Float ) -> State -> Form
displayQuadrants ( w, h ) state =
    let
        grid =
            group
                [ path [ ( 0, 0 ), ( 0, -h / 2 ) ]
                    |> traced (solid viewCfg.quadrantCol)
                , path [ ( -w / 2, 0 ), ( w / 2, 0 ) ]
                    |> traced (solid viewCfg.quadrantCol)
                ]
    in
        if state == Serve then
            grid
        else
            noForm


displayForeground : Model -> Element
displayForeground ({ state, gameBall, player, spareBalls } as model) =
    let
        pointsMsg =
            pointsText (List.length model.bricks) spareBalls model.contacts

        spareBallsMsg =
            "spare balls: " ++ toString spareBalls

        ball =
            circle gameBall.r |> make lightGray gameBall

        paddle =
            rect player.w player.h |> make darkGray player

        serveTextForm =
            if state == Serve then
                txt identity viewCfg.manualMsg
                    |> toForm
                    |> move ( 0, viewCfg.msgTextPosY )
            else
                noForm

        endMsg =
            case state of
                Won ->
                    viewCfg.wonMsg

                Lost ->
                    viewCfg.lostMsg

                _ ->
                    ""

        showEndText =
            state == Won || state == Lost

        endText =
            txt (Text.height viewCfg.endTextHeight)
                (pointsMsg ++ "\n" ++ endMsg)

        endTextForm =
            if showEndText then
                endText |> toForm
            else
                noForm

        quadrants =
            displayQuadrants ( modelCfg.gameWidth, modelCfg.gameHeight ) state

        pointsTextForm =
            txt identity pointsMsg |> toForm |> move viewCfg.pointsTextPos

        spareBallsForm =
            txt identity spareBallsMsg
                |> toForm
                |> move viewCfg.spareBallsTxtPos
    in
        displayFullScreen model.windowDimensions
            <| group
                [ paddle
                , ball
                , serveTextForm
                , pointsTextForm
                , spareBallsForm
                , endTextForm
                , quadrants
                ]
