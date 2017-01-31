module App exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Vector2 as V2 exposing (distance, normalize, setX, getX, getY)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)
import Touch exposing (TouchEvent(..), Touch)
import MultiTouch exposing (MultiTouch, onMultiTouch)
import AnimationFrame
import Window
import Task
import Dict exposing (values)
import Player exposing (updatePlayer, fireFromBarrel)
import GameTypes exposing (Model, Barrel, Player, Vector, DeltaTime, CreateMode(..), Force(..), PlayTestControles(..), ActiveElement(..))
import Draw exposing (render, renderPlayer, renderBarrel, renderTouch)
import Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits, gameSize)
import Forces exposing (moveLeft, moveRight, dontMove)


initialModel : Model
initialModel =
    let
        startingPoint =
            ( 0, 0 )
    in
        { canvasSize = ( 0, 0 )
        , player = Player startingPoint ( 0, 0 ) 45
        , barrels = [ Barrel ( 0, -100 ) (3 * pi / 4) 45, Barrel ( 200, -100 ) (pi / 4) 45 ]
        , active = ThePlayer
        , force = GoWithTheFlow
        , camera = Camera.fixedWidth (getX gameSize) startingPoint
        , resources = Resources.init
        , touchLocations = []
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
    | MultiTouchMsg MultiTouch
    | Resources Resources.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            model ! []

        SetCanvasSize size ->
            { model
                | canvasSize = setCanvasSize size
            }
                ! []

        Tick dt ->
            let
                buttonsPressed =
                    calculateButtonsPressed model.mode model.touchLocations
            in
                { model
                    | player = updatePlayer dt model.active model.player model.force
                    , active = calculateActiveElement model.player model.barrels
                    , camera = Camera.follow 0.5 0.17 (V2.sub model.player.location ( -100, -100 )) model.camera
                }
                    ! []

        Resources msg ->
            { model | resources = Resources.update msg model.resources }
                ! []

        MultiTouchMsg multiTouch ->
            { model
                | touchLocations =
                    values multiTouch.touches
                        |> List.map (\{ clientX, clientY } -> ( clientX, clientY ))
                        |> List.map (convertToGameUnits model.canvasSize)
            }
                ! []


calculateButtonsPressed : CreateMode -> List Vector -> List PlayTestControles
calculateButtonsPressed mode touchLocations =
    let
        buttonPressed =
            case mode of
                PlayTest ->
                    calculatePlayTestControles touchLocations

                Edit ->
                    calculatePlayTestControles touchLocations
    in
        [ buttonPressed ]


calculatePlayTestControles : List Vector -> PlayTestControles
calculatePlayTestControles touchLocations =
    case touchLocations of
        ( x, y ) :: rest ->
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
    div []
        [ Game.renderCenteredWithOptions
            []
            (onAllTouch
                ++ [ style [ ( "border", "solid 1px black" ) ] ]
            )
            { time = 0
            , size = ( floor <| getX model.canvasSize, floor <| getY model.canvasSize )
            , camera = model.camera
            }
            (render model)
          -- test touch
          -- , div
          --     [ style [ ( "font-size", "72px" ), ( "position", "relative" ), ( "top", "-100" ), ( "left", "-100" ) ]
          --     ]
          --     [ Html.text <| toString <| model.touchLocations ]
        ]


setCanvasSize : Window.Size -> ( Float, Float )
setCanvasSize size =
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
    [ onMultiTouch TouchStart Touch.preventAndStop <| MultiTouchMsg
    , onMultiTouch TouchMove Touch.preventAndStop <| MultiTouchMsg
    , onMultiTouch TouchEnd Touch.preventAndStop <| MultiTouchMsg
    , onMultiTouch TouchCancel Touch.preventAndStop <| MultiTouchMsg
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs <| Tick << (\dt -> dt / 1000)
        , Window.resizes (\size -> SetCanvasSize size)
        ]
