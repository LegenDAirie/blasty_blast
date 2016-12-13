module App exposing (..)

import Html exposing (Html, div)
import Collage exposing (collage)
import Element exposing (toHtml)
import Vector2 as V2
import AnimationFrame
import Window
import Task
import Draw exposing (sizeCanvas, backgroundColor, drawPlayer, drawBarrel)


type alias Vector =
    ( Float, Float )


type alias Player =
    { location : Vector
    , velocity : Vector
    }


type alias Barrel =
    { location : Vector
    , angle : Float
    }


type alias Model =
    { canvasSize : Window.Size
    , player : Player
    , barrel : Barrel
    }


type alias DeltaTime =
    Float


initialModel : Model
initialModel =
    { canvasSize = { width = 0, height = 0 }
    , player = Player ( -100, 100 ) ( 0, 0 )
    , barrel = Barrel ( -100, -100 ) <| pi / 4
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> SetCanvasSize size) Window.size )


type Msg
    = NoOp
    | SetCanvasSize Window.Size
    | Tick DeltaTime


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetCanvasSize size ->
            ( { model
                | canvasSize = size
              }
            , Cmd.none
            )

        Tick dt ->
            ( { model
                | player = updatePlayer dt model.player
              }
            , Cmd.none
            )


updatePlayer : DeltaTime -> Player -> Player
updatePlayer dt player =
    let
        gravity =
            V2.scale dt ( 0, -0.1 )

        newVelocity =
            V2.add player.velocity gravity
    in
        { player
            | location = V2.add player.location newVelocity
            , velocity = newVelocity
        }


view : Model -> Html Msg
view model =
    let
        ( canvasWidth, canvasHeight ) =
            sizeCanvas model.canvasSize

        gameScale =
            toFloat canvasWidth / 1280
    in
        div []
            [ toHtml <|
                collage
                    canvasWidth
                    canvasHeight
                    [ backgroundColor canvasWidth canvasHeight
                    , drawPlayer model.player gameScale
                    , drawBarrel model.barrel gameScale
                    ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Window.resizes (\size -> SetCanvasSize size)
        ]
