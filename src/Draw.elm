module Draw exposing (..)

import Collage exposing (Form, rect, circle, traced, filled, move, moveX, rotate, group, solid, outlined)
import Transform exposing (rotation)
import Color exposing (rgb, white, black)
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


canvasBackground : Form
canvasBackground =
    rect 1280 720
        |> filled white


center : Vector -> Form
center location =
    circle 1
        |> filled black
        |> move location


collisionCircle : Vector -> Int -> Int -> Form
collisionCircle location width height =
    circle (toFloat height / 2)
        |> outlined (solid black)
        |> move location


drawPlayer : Player -> Form
drawPlayer player =
    group
        [ rect 75 75
            |> outlined (solid (rgb 255 128 128))
            |> move player.location
        , center player.location
        , collisionCircle player.location 75 75
        ]


drawBarrel : Barrel -> Form
drawBarrel barrel =
    group
        [ rect 100 75
            |> outlined (solid (rgb 60 100 60))
            |> move barrel.location
        , center barrel.location
        , collisionCircle barrel.location 100 75
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
