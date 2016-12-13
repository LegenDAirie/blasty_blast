module Draw exposing (..)

import Collage exposing (Form, rect, filled, move, scale, rotate)
import Color exposing (rgb, darkCharcoal)
import Vector2 as V2
import Window


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
