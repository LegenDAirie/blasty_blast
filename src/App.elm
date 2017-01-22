module App exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Vector2 as V2 exposing (distance, normalize, setX)
import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import AnimationFrame
import Window
import Task
import Player exposing (Player, updatePlayer)
import Barrel exposing (updateBarrel)
import GameTypes exposing (Barrel, Vector, Controles(..), ActiveElement(..))
import Draw exposing (renderPlayer, renderBarrel)
import Color


type alias Model =
    { windowSize : Window.Size
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , move : Controles
    , camera : Camera
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
    , camera = Camera.fixedWidth 1280 ( 0, 0 )
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
            let
                _ =
                    Debug.log "dt" dt
            in
                ( { model
                    | player = updatePlayer dt model.active model.player model.move
                    , active = calculateActiveElement model.player model.barrels
                    , camera =
                        Camera.follow 0.5 0.17 model.player.location model.camera
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

        canvasContainer =
            style
                [ ( "display", "flex" ) ]
    in
        div []
            [ Game.renderCentered
                { time = 0
                , size = sizeCanvas model.windowSize
                , camera = model.camera
                }
                (render model)
            ]


render : Model -> List Renderable
render model =
    List.concat
        [ [ renderPlayer model.player ]
        , (List.map renderBarrel model.barrels)
        ]


sizeCanvas : Window.Size -> ( Int, Int )
sizeCanvas size =
    let
        width =
            min size.width <|
                floor (16 / 9 * toFloat size.height)

        height =
            min size.height <|
                floor (9 / 16 * toFloat size.width)
    in
        ( width, height )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes (\size -> SetCanvasSize size)
        ]
