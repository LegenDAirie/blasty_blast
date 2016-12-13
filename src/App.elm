module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style, id)
import Collage exposing (collage, rect, filled, move, scale, rotate, Form)
import Element exposing (toHtml)
import Vector2 as V2 exposing (Vec2, Float2)
import Color exposing (rgb, darkCharcoal)
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
    { message : String
    , canvasSize : Window.Size
    , player : Player
    , barrel : Barrel
    }


initialModel : Model
initialModel =
    { message = "Your Elm App is working!"
    , canvasSize = { width = 0, height = 0 }
    , player = Player ( -100, 100 ) ( 0, 0 )
    , barrel = Barrel ( -100, -100 ) <| pi / 4
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> SetCanvasSize size) Window.size )


type Msg
    = NoOp
    | SetCanvasSize Window.Size


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


view : Model -> Html Msg
view model =
    let
        ( canvasWidth, canvasHeight ) =
            sizeCanvas model.canvasSize

        gameElementScale =
            toFloat canvasWidth / 1280

        location =
            model.player.location

        ( barrelX, barrelY ) =
            model.barrel.location
    in
        div []
            [ toHtml <|
                collage canvasWidth
                    canvasHeight
                    [ backgroundColor canvasWidth canvasHeight
                    , drawPlayer model.player gameElementScale
                    , drawBarrel model.barrel gameElementScale
                    ]
            ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\size -> SetCanvasSize size)
