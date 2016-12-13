module App exposing (..)

import Html exposing (Html, text, div)
import Html.Attributes exposing (style, id)
import Collage exposing (collage, rect, filled, move, scale, rotate, Form)
import Element exposing (toHtml)
import Vector2 as V2 exposing (Vec2, Float2)
import Color exposing (rgb, darkCharcoal)
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
    , player = Player ( -100, 100 ) ( 0, 0 )
    , barrel = Barrel ( -100, -100 ) <| pi / 4
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
            sizeCanvas model.svgCanvasSize

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


backgroundColor : Int -> Int -> Form
backgroundColor width height =
    rect (toFloat width) (toFloat height)
        |> filled darkCharcoal


drawPlayer : Player -> Float -> Form
drawPlayer player elementScale =
    rect 75 75
        |> filled (rgb 60 100 60)
        |> move (V2.scale elementScale player.location)
        |> scale elementScale


drawBarrel : Barrel -> Float -> Form
drawBarrel barrel elementScale =
    rect 100 75
        |> filled (rgb 60 100 60)
        |> move (V2.scale elementScale barrel.location)
        |> scale elementScale
        |> rotate (pi / 4)


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
    Window.resizes (\size -> SetSvgCanvasSize size)
