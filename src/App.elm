module App exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Vector2 as V2 exposing (distance, normalize, setX, getX, getY)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)
import Touch exposing (TouchEvent(..), Touch)
import SingleTouch exposing (SingleTouch, onSingleTouch)
import AnimationFrame
import Window
import Task
import Color
import Player exposing (updatePlayer, fireFromBarrel)
import Barrel exposing (updateBarrel)
import GameTypes exposing (Model, Barrel, Player, Vector, DeltaTime, CreateMode(..), Force(..), PlayTestControles(..), ActiveElement(..))
import Draw exposing (render, renderPlayer, renderBarrel, renderTouch)
import Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits)
import Forces exposing (moveLeft, moveRight, dontMove)


initialModel : Model
initialModel =
    let
        startingPoint =
            ( 0, 0 )
    in
        { windowSize = { width = 0, height = 0 }
        , player = Player startingPoint ( 0, 0 ) 45
        , barrels = [ Barrel ( 0, -100 ) (3 * pi / 4) 45, Barrel ( 200, -100 ) (pi / 4) 45 ]
        , active = ThePlayer
        , force = GoWithTheFlow
        , camera = Camera.fixedWidth 1280 startingPoint
        , resources = Resources.init
        , touchLocation = ( 0, 0 )
        , mode = PlayTest
        , debug = ""
        }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetCanvasSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/ghost-friend.png" ]
          ]


type Msg
    = NoOp
    | SetCanvasSize Window.Size
    | Tick DeltaTime
    | SingleTouchMsg SingleTouch
    | Resources Resources.Msg


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
                , camera = Camera.follow 0.5 0.17 (V2.sub model.player.location ( -100, -100 )) model.camera
            }
                ! []

        Resources msg ->
            { model | resources = Resources.update msg model.resources }
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

                newModel =
                    case model.mode of
                        PlayTest ->
                            let
                                buttonPressed =
                                    calculatePlayTestControles touchEvent.touchType touchScreenLocation
                            in
                                case buttonPressed of
                                    Left ->
                                        moveLeft model

                                    Right ->
                                        moveRight model

                                    Fire ->
                                        fire model

                                    -- SwitchToEditMode ->
                                    --     { model |
                                    --         mode = Edit
                                    --     }
                                    None ->
                                        dontMove model

                        Edit ->
                            let
                                buttonPressed =
                                    calculatePlayTestControles touchEvent.touchType touchScreenLocation
                            in
                                case buttonPressed of
                                    Left ->
                                        moveLeft model

                                    Right ->
                                        moveRight model

                                    Fire ->
                                        fire model

                                    -- SwitchToEditMode ->
                                    --     { model |
                                    --         mode = Edit
                                    --     }
                                    None ->
                                        dontMove model
            in
                { newModel
                    | touchLocation = newTouchLocation
                }
                    ! []


calculatePlayTestControles : TouchEvent -> Vector -> PlayTestControles
calculatePlayTestControles touchType ( x, y ) =
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
                [ Html.text <| toString <| getX <| model.player.location ]
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
