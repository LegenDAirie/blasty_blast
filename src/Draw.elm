module Draw exposing (..)

import Collage exposing (Form, rect, circle, filled, move, scale, rotate, group, solid, outlined)
import Color exposing (rgb, darkCharcoal, white, black)
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
        |> filled white


center : Float -> Vector -> Form
center elementScale location =
    circle 1
        |> filled black
        |> move (V2.scale elementScale location)


collisionCircle : Float -> Vector -> Int -> Int -> Form
collisionCircle elementScale location width height =
    circle (toFloat height / 2)
        |> outlined (solid black)
        |> move (V2.scale elementScale location)
        |> scale elementScale


drawPlayer : Player -> Float -> Form
drawPlayer player elementScale =
    group
        [ rect 75 75
            |> outlined (solid (rgb 255 128 128))
            |> move (V2.scale elementScale player.location)
            |> scale elementScale
        , center elementScale player.location
        , collisionCircle elementScale player.location 75 75
        ]


drawBarrel : Barrel -> Float -> Form
drawBarrel barrel elementScale =
    group
        [ rect 100 75
            |> outlined (solid (rgb 60 100 60))
            |> move (V2.scale elementScale barrel.location)
            |> scale elementScale
            |> rotate (pi / 4)
        , center elementScale barrel.location
        , collisionCircle elementScale barrel.location 100 75
        ]


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
