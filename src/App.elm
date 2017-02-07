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
import GameTypes exposing (Barrel, Player, Vector, DeltaTime, ActiveElement(..))
import Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits, gameSize)
import Screens.LevelCreationScreen exposing (initialLevelCreateState, LevelCreationMode(..), levelCreateScreenUpdate, renderLevelCreation, LevelCreateState)


type alias Model =
    { canvasSize : Vector
    , touchLocations : List Vector
    , gameScreen : GameScreen
    }


type GameScreen
    = Uninitialized
    | LevelCreateScreen LevelCreateState


type Msg
    = SetCanvasSize Window.Size
    | Tick DeltaTime
    | MultiTouchMsg MultiTouch
    | Resources Resources.Msg


initialModel : Model
initialModel =
    { canvasSize = ( 0, 0 )
    , touchLocations = []
    , gameScreen = LevelCreateScreen initialLevelCreateState
    }


init : ( Model, Cmd Msg )
init =
    initialModel
        ! [ Task.perform SetCanvasSize Window.size
          , Cmd.map Resources <| Resources.loadTextures [ "../assets/ghost-friend.png" ]
          ]


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
                        | gameScreen =
                            LevelCreateScreen <|
                                levelCreateScreenUpdate
                                    deltaTime
                                    model.touchLocations
                                    state
                    }
                        ! []


view : Model -> Html Msg
view model =
    let
        ( camera, gameScene ) =
            case model.gameScreen of
                Uninitialized ->
                    ( Camera.fixedWidth (getX gameSize) ( 0, 0 ), [] )

                LevelCreateScreen state ->
                    renderLevelCreation state
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
