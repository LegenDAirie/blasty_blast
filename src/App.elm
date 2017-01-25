module App exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Vector2 as V2 exposing (distance, normalize, setX, getX, getY)
import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)
import Touch exposing (TouchEvent(..), Touch)
import SingleTouch exposing (SingleTouch, onSingleTouch)
import AnimationFrame
import Window
import Task
import Color
import Player exposing (updatePlayer)
import Barrel exposing (updateBarrel)
import GameTypes exposing (Barrel, Player, Vector, Force(..), Controles(..), ActiveElement(..))
import Draw exposing (renderPlayer, renderBarrel, renderTouch)
import Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits)
import Forces exposing (moveLeft, moveRight, dontMove)


type alias Model =
    { windowSize : Window.Size
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , force : Force
    , camera : Camera
    , touchLocation : Vector
    , debug : String
    }


type alias DeltaTime =
    Float


initialModel : Model
initialModel =
    let
        startingPoint =
            ( -300, 100 )
    in
        { windowSize = { width = 0, height = 0 }
        , player = Player startingPoint ( 0, 0 ) 35
        , barrels = [ Barrel ( -300, -100 ) (pi / 4) 35, Barrel ( 300, -100 ) (3 * pi / 4) 35 ]
        , active = ThePlayer
        , force = GoWithTheFlow
        , camera = Camera.fixedWidth 1280 startingPoint
        , touchLocation = ( 0, 0 )
        , debug = ""
        }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform SetCanvasSize Window.size )


type Msg
    = NoOp
    | SetCanvasSize Window.Size
    | Tick DeltaTime
    | SingleTouchMsg SingleTouch


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetCanvasSize size ->
            { model
                | windowSize = size
            }
                ! []

        Tick dt ->
            { model
                | player = updatePlayer dt model.active model.player model.force
                , active = calculateActiveElement model.player model.barrels
                , camera = Camera.follow 0.5 0.17 model.player.location model.camera
            }
                ! []

        SingleTouchMsg touchEvent ->
            let
                { clientX, clientY } =
                    touchEvent.touch

                ( canvasWidth, canvasHeight ) =
                    sizeCanvas model.windowSize

                gameSize =
                    getViewSize (sizeCanvas model.windowSize) model.camera

                sizeRatio =
                    getX gameSize / canvasWidth

                newTouchLocation =
                    convertTouchCoorToGameCoor gameSize sizeRatio model.camera ( clientX, clientY )

                touchScreenLocation =
                    ( clientX, clientY )
                        |> convertToGameUnits sizeRatio

                buttonPressed =
                    calculateControles touchEvent.touchType touchScreenLocation

                newModel =
                    case buttonPressed of
                        Left ->
                            moveLeft model

                        Right ->
                            moveRight model

                        Fire ->
                            fire model

                        None ->
                            dontMove model
            in
                { newModel
                    | touchLocation = newTouchLocation
                }
                    ! []


calculateControles : TouchEvent -> Vector -> Controles
calculateControles touchType ( x, y ) =
    case touchType of
        TouchStart ->
            if x < 320 then
                Left
            else if x >= 320 && x < 960 then
                Fire
            else if x > 960 then
                Right
            else
                None

        _ ->
            None


fire : Model -> Model
fire model =
    case model.active of
        ThePlayer ->
            model

        ThisBarrel barrel ->
            { model
                | player = fireFromBarrel barrel model.player
                , active = ThePlayer
            }


fireFromBarrel : Barrel -> Player -> Player
fireFromBarrel barrel player =
    let
        minDistanceApart =
            toFloat (barrel.collisionRadius + player.collisionRadius)

        directionVector =
            ( cos barrel.angle, sin barrel.angle )

        newLocatioin =
            directionVector
                |> V2.scale minDistanceApart
                |> V2.add barrel.location

        newVelocity =
            directionVector
                |> V2.scale 10
    in
        { player
            | location = newLocatioin
            , velocity = newVelocity
        }


calculateActiveElement : Player -> List Barrel -> ActiveElement
calculateActiveElement player barrels =
    case barrels of
        barrel :: rest ->
            if hasCollided player barrel then
                ThisBarrel barrel
            else
                calculateActiveElement player rest

        [] ->
            ThePlayer


hasCollided : Player -> Barrel -> Bool
hasCollided player barrel =
    let
        distanceBetween =
            distance player.location barrel.location

        collectiveRadius =
            player.collisionRadius + barrel.collisionRadius
    in
        distanceBetween < toFloat collectiveRadius


view : Model -> Html Msg
view model =
    let
        ( canvasWidth, canvasHeight ) =
            sizeCanvas model.windowSize

        -- ( touchX, touchY ) =
        --     ( getX model.touchLocation, getY model.touchLocation )
    in
        div []
            [ Game.renderCenteredWithOptions
                []
                (onAllTouch
                    ++ [ style [ ( "border", "solid 1px black" ) ] ]
                )
                { time = 0
                , size = ( floor canvasWidth, floor canvasHeight )
                , camera = model.camera
                }
                (render model)
              -- test touch
            , div
                [ style [ ( "font-size", "72px" ), ( "position", "relative" ), ( "top", "-100" ), ( "left", "-100" ) ]
                ]
                [ Html.text <| model.debug ]
            ]


render : Model -> List Renderable
render model =
    List.concat
        [ [ renderPlayer model.player ]
        , (List.map renderBarrel model.barrels)
        , [ renderTouch model.touchLocation model.camera ]
        ]


sizeCanvas : Window.Size -> ( Float, Float )
sizeCanvas size =
    let
        width =
            min size.width <|
                floor (16 / 9 * toFloat size.height)

        height =
            min size.height <|
                floor (9 / 16 * toFloat size.width)
    in
        ( toFloat width, toFloat height )


onAllTouch : List (Html.Attribute Msg)
onAllTouch =
    [ onSingleTouch TouchStart Touch.preventAndStop <| SingleTouchMsg
    , onSingleTouch TouchMove Touch.preventAndStop <| SingleTouchMsg
    , onSingleTouch TouchEnd Touch.preventAndStop <| SingleTouchMsg
    , onSingleTouch TouchCancel Touch.preventAndStop <| SingleTouchMsg
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs <| Tick << (\dt -> dt / 1000)
        , Window.resizes (\size -> SetCanvasSize size)
        ]
