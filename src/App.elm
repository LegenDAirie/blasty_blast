module App exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseDown, onMouseUp)
import Collage exposing (collage, groupTransform)
import Transform exposing (identity, rotation, translation)
import Element exposing (toHtml)
import Vector2 as V2 exposing (distance, normalize, setX)
import AnimationFrame
import Window
import Task
import Draw exposing (sizeCanvas, canvasBackground, drawPlayer, drawBarrel)
import Player exposing (Player, updatePlayer)
import GameTypes exposing (Barrel, Vector, DeltaTime, Controles(..), ActiveElement(..))


type alias Model =
    { windowSize : Window.Size
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , move : Controles
    }


initialModel : Model
initialModel =
    { windowSize = { width = 0, height = 0 }
    , player = Player ( -300, 100 ) ( 0, 0 ) 35
    , barrels = [ Barrel ( -300, -100 ) (pi / 4) 35, Barrel ( 300, -100 ) (3 * pi / 4) 35 ]
    , active = ThePlayer
    , move = GoWithTheFlow
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> SetCanvasSize size) Window.size )


type Msg
    = NoOp
    | SetCanvasSize Window.Size
    | Tick DeltaTime
    | MoveLeft
    | MoveRight
    | DontMove
    | Fire


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetCanvasSize size ->
            ( { model
                | windowSize = size
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | player = updatePlayer dt model.active model.player model.move
                , active = calculateActiveElement model.player model.barrels
              }
            , Cmd.none
            )

        MoveLeft ->
            case model.active of
                ThePlayer ->
                    ( { model
                        | move = GoLeft
                      }
                    , Cmd.none
                    )

                ThisBarrel barrel ->
                    ( { model
                        | barrels = updateBarrels model.barrels barrel (pi / 4)
                      }
                    , Cmd.none
                    )

        MoveRight ->
            case model.active of
                ThePlayer ->
                    ( { model
                        | move = GoRight
                      }
                    , Cmd.none
                    )

                ThisBarrel barrel ->
                    ( { model
                        | barrels = updateBarrels model.barrels barrel (-pi / 4)
                      }
                    , Cmd.none
                    )

        DontMove ->
            ( { model
                | move = GoWithTheFlow
              }
            , Cmd.none
            )

        Fire ->
            case model.active of
                ThePlayer ->
                    ( model, Cmd.none )

                ThisBarrel barrel ->
                    ( { model
                        | player = fireFromBarrel barrel model.player
                        , active = ThePlayer
                      }
                    , Cmd.none
                    )


updateBarrels : List Barrel -> Barrel -> Float -> List Barrel
updateBarrels barrels barrelToUpdate newAngle =
    case barrels of
        barrel :: rest ->
            if barrel == barrelToUpdate then
                turnBarrel barrel newAngle :: rest
            else
                barrel :: updateBarrels rest barrelToUpdate newAngle

        [] ->
            []


turnBarrel : Barrel -> Float -> Barrel
turnBarrel barrel offsetAngle =
    { barrel
        | angle = barrel.angle + offsetAngle
    }


fireFromBarrel : Barrel -> Player -> Player
fireFromBarrel barrel player =
    let
        minDistanceApart =
            toFloat (barrel.collisionRadius + player.collisionRadius)

        unitVector =
            ( cos barrel.angle, sin barrel.angle )

        newLocatioin =
            unitVector
                |> V2.scale minDistanceApart
                |> V2.add barrel.location

        newVelocity =
            unitVector
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


capHorizontalVelocity : Float -> Vector -> Vector
capHorizontalVelocity maxSpeed ( x, y ) =
    if x > maxSpeed then
        ( maxSpeed, y )
    else if x < -maxSpeed then
        ( -maxSpeed, y )
    else
        ( x, y )


capVerticalVelocity : Float -> Vector -> Vector
capVerticalVelocity maxSpeed ( x, y ) =
    if y < -maxSpeed then
        ( x, -maxSpeed )
    else
        ( x, y )


view : Model -> Html Msg
view model =
    let
        ( canvasWidth, canvasHeight ) =
            sizeCanvas model.windowSize

        fireButtonWidth =
            toFloat canvasWidth * 0.7

        leftRightButtonWidth =
            toFloat canvasWidth * 0.15

        gameScale =
            toFloat canvasHeight / 720

        gameTransformation =
            Transform.scale gameScale

        fireButtonStyle =
            style
                [ ( "position", "absolute" )
                , ( "left", toString leftRightButtonWidth ++ "px" )
                , ( "width", toString fireButtonWidth ++ "px" )
                , ( "height", toString canvasHeight ++ "px" )
                , ( "border", "1px solid red" )
                ]

        leftButtonStyle =
            style
                [ ( "position", "absolute" )
                , ( "width", toString leftRightButtonWidth ++ "px" )
                , ( "height", toString canvasHeight ++ "px" )
                , ( "border", "1px solid red" )
                ]

        rightButtonStyle =
            style
                [ ( "position", "absolute" )
                , ( "left", toString (leftRightButtonWidth + fireButtonWidth) ++ "px" )
                , ( "width", toString leftRightButtonWidth ++ "px" )
                , ( "height", toString canvasHeight ++ "px" )
                , ( "border", "1px solid red" )
                ]

        canvasContainer =
            style
                [ ( "display", "flex" ) ]
    in
        div []
            [ div [ canvasContainer ]
                [ toHtml <|
                    collage
                        canvasWidth
                        canvasHeight
                        [ groupTransform gameTransformation
                            (canvasBackground
                                :: (drawPlayer model.player)
                                :: (List.map drawBarrel model.barrels)
                            )
                        ]
                , div [ leftButtonStyle, onMouseDown MoveLeft, onMouseUp DontMove ]
                    []
                , div [ fireButtonStyle, onMouseDown Fire ]
                    []
                , div [ rightButtonStyle, onMouseDown MoveRight, onMouseUp DontMove ]
                    []
                ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes (\size -> SetCanvasSize size)
        ]
