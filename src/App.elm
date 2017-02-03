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
import GameTypes exposing (GameScreen(..), LevelCreateState, Model, Barrel, Player, Vector, DeltaTime, LevelCreationMode(..), ActiveElement(..))
import GameLogic exposing (calculateActiveElement)
import Draw exposing (render, renderPlayer, renderBarrel, renderTouch)
import Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits, gameSize)
import Controls exposing (calculatePlayTestControls)


initialModel : Model
initialModel =
    { canvasSize = ( 0, 0 )
    , touchLocations = []
    , gameScreen = LevelCreateScreen initialLevelCreateState
    }


initialLevelCreateState : LevelCreateState
initialLevelCreateState =
    let
        startingPoint =
            ( 0, 0 )
    in
        { player = Player startingPoint ( 0, 0 ) 45
        , barrels = [ Barrel ( 0, -100 ) (3 * pi / 4) 45, Barrel ( 200, -100 ) (pi / 4) 45 ]
        , activeElement = ThePlayer
        , camera = Camera.fixedWidth (getX gameSize) startingPoint
        , resources = Resources.init
        , levelCreationMode = PlayTest
        , debug = ""
        }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetCanvasSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/ghost-friend.png" ]
          ]


type Msg
    = SetCanvasSize Window.Size
    | Tick DeltaTime
    | MultiTouchMsg MultiTouch
    | Resources Resources.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetCanvasSize size ->
            { model | canvasSize = setCanvasSize size }
                ! []

        MultiTouchMsg multiTouch ->
            { model
                | touchLocations =
                    values multiTouch.touches
                        |> List.map (\{ clientX, clientY } -> ( clientX, clientY ))
                        |> List.map (convertToGameUnits model.canvasSize)
            }
                ! []

        Resources msg ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                LevelCreateScreen state ->
                    { model
                        | gameScreen =
                            LevelCreateScreen { state | resources = Resources.update msg state.resources }
                    }
                        ! []

        Tick deltaTime ->
            case model.gameScreen of
                Uninitialized ->
                    model ! []

                LevelCreateScreen state ->
                    { model
                        | gameScreen = levelCreateScreenUpdate deltaTime model.touchLocations state
                    }
                        ! []


levelCreateScreenUpdate : DeltaTime -> List Vector -> LevelCreateState -> GameScreen
levelCreateScreenUpdate deltaTime touchLocations state =
    let
        pressedButtons =
            calculatePlayTestControls touchLocations

        activeElement =
            calculateActiveElement state.player state.barrels

        ( newActiveElement, newPlayer ) =
            updatePlayer deltaTime activeElement pressedButtons state.player
    in
        LevelCreateScreen
            { state
                | player = newPlayer
                , camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
                , activeElement = newActiveElement
            }


view : Model -> Html Msg
view model =
    let
        ( camera, gameScene ) =
            render model
    in
        div []
            [ Game.renderCenteredWithOptions
                []
                (onAllTouch
                    ++ [ style [ ( "border", "solid 1px black" ) ] ]
                )
                { time = 0
                , size = ( floor <| getX model.canvasSize, floor <| getY model.canvasSize )
                , camera = camera
                }
                gameScene
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
