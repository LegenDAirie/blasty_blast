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
import Player exposing (Player, updatePlayer)
import Barrel exposing (updateBarrel)
import GameTypes exposing (Barrel, Vector, Controles(..), ActiveElement(..))
import Draw exposing (renderPlayer, renderBarrel, renderTouch)
import Color


type alias Model =
    { windowSize : Window.Size
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , move : Controles
    , camera : Camera
    , touchLocation : Vector
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
    , touchLocation = ( 0, 0 )
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform SetCanvasSize Window.size )


type Msg
    = NoOp
    | SetCanvasSize Window.Size
    | Tick DeltaTime
    | MoveLeft
    | MoveRight
    | DontMove
    | Fire
    | SingleTouchMsg SingleTouch


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
                , camera = Camera.follow 0.5 0.17 model.player.location model.camera
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
            in
                ( { model | touchLocation = newTouchLocation }
                , Cmd.none
                )


convertTouchCoorToGameCoor : Vector -> Float -> Camera -> Vector -> Vector
convertTouchCoorToGameCoor gameSize sizeRatio camera touchLocation =
    touchLocation
        |> convertToGameUnits sizeRatio
        |> offSetOrigin gameSize
        |> offSetByCamera camera
        |> flipY


flipY : Vector -> Vector
flipY ( x, y ) =
    ( x, -y )


convertToGameUnits : Float -> Vector -> Vector
convertToGameUnits sizeRatio touchLocation =
    V2.scale sizeRatio touchLocation


offSetOrigin : Vector -> Vector -> Vector
offSetOrigin gameSize touchLocation =
    gameSize
        |> V2.scale 0.5
        |> V2.sub touchLocation


offSetByCamera : Camera -> Vector -> Vector
offSetByCamera camera touchLocation =
    camera
        |> getPosition
        |> flipY
        |> V2.add touchLocation


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

        -- test touch
        -- ( gameWidth, gameHeight ) =
        --     getViewSize ( canvasWidth, canvasHeight ) model.camera
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
              -- , div [ style [ ( "width", "50px" ), ( "height", "50px" ), ( "border", "solid 1px black" ), ( "position", "absolute" ), ( "top", "0" ), ( "left", "0" ) ] ] [ Html.text <| toString ( touchX, touchY ) ]
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
