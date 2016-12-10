module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (x, y, width, height, viewBox)
import Window
import Task


type alias Model =
    { message : String
    , svgCanvasSize : Window.Size
    }


initialModel : Model
initialModel =
    { message = "Your Elm App is working!"
    , svgCanvasSize = { width = 0, height = 0 }
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
        ( svgCanvasWidth, svgCanvasHeight ) =
            sizeSvgCanvas model.svgCanvasSize
    in
        div []
            [ svg
                [ width svgCanvasWidth, height svgCanvasHeight, viewBox "0 0 1280 720" ]
                [ rect
                    [ x "500"
                    , y "500"
                    , width "100"
                    , height "100"
                    ]
                    []
                ]
            ]


sizeSvgCanvas : Window.Size -> ( String, String )
sizeSvgCanvas size =
    let
        width =
            toString <|
                min size.width <|
                    floor (16 / 9 * toFloat size.height)

        height =
            toString <|
                min size.height <|
                    floor (9 / 16 * toFloat size.width)
    in
        ( width, height )


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\size -> SetSvgCanvasSize size)
