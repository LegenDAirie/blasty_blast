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
import Barrel exposing (updateBarrel)
import GameTypes exposing (Barrel, Vector, Controles(..), ActiveElement(..))


type alias Model =
    { windowSize : Window.Size
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , move : Controles
    }


type alias DeltaTime =
    Float


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
                    let
                        transformBarrel =
                            Barrel.rotate (pi / 4)
                    in
                        ( { model
                            | barrels = updateBarrel transformBarrel model.barrels barrel
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
                    let
                        fn =
                            Barrel.rotate (-pi / 4)
                    in
                        ( { model
                            | barrels = updateBarrel fn model.barrels barrel
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
