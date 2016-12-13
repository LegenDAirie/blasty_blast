module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Collage exposing (collage, rect, filled, move, scale, rotate)
import Element exposing (toHtml)
import Vector2 as V2 exposing (Vec2, Float2)
import Color exposing (rgb)
import Window
import Task


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
    , svgCanvasSize : Window.Size
    , player : Player
    , barrel : Barrel
    }


initialModel : Model
initialModel =
    { message = "Your Elm App is working!"
    , svgCanvasSize = { width = 0, height = 0 }
    , player = Player ( 0, 0 ) ( 0, 0 )
    , barrel = Barrel ( 200, 400 ) <| pi / 4
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform (\size -> SetSvgCanvasSize size) Window.size )


type Msg
    = NoOp
    | SetSvgCanvasSize Window.Size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetSvgCanvasSize size ->
            ( { model
                | svgCanvasSize = size
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    let
        ( canvasWidth, canvasHeight ) =
            sizeSvgCanvas model.svgCanvasSize

        location =
            model.player.location

        ( barrelX, barrelY ) =
            model.barrel.location
    in
        div [ style [ ( "backgroundColor", "#333" ) ] ]
            [ toHtml <|
                collage canvasWidth
                    canvasHeight
                    [ rect 75 75
                        |> filled (rgb 60 100 60)
                        |> move location
                        |> scale (toFloat canvasWidth / 1280)
                        |> rotate (pi / 4)
                    ]
            ]


sizeSvgCanvas : Window.Size -> ( Int, Int )
sizeSvgCanvas size =
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
    Window.resizes (\size -> SetSvgCanvasSize size)
