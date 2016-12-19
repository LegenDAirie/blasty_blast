module Draw exposing (..)

import Collage exposing (Form, rect, circle, traced, filled, move, moveX, rotate, group, solid, outlined)
import Color exposing (rgb, white, black)
import Window


type alias Vector =
    ( Float, Float )


type alias Player =
    { location : Vector
    , velocity : Vector
    , collisionRadius : Int
    }


type alias Barrel =
    { location : Vector
    , angle : Float
    , collisionRadius : Int
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


collisionCircle : Vector -> Int -> Form
collisionCircle location radius =
    toFloat radius
        |> circle
        |> outlined (solid black)
        |> move location


drawPlayer : Player -> Form
drawPlayer player =
    group
        [ rect 75 75
            |> outlined (solid (rgb 255 128 128))
            |> move player.location
        , center player.location
        , collisionCircle player.location player.collisionRadius
        ]


drawBarrel : Barrel -> Form
drawBarrel barrel =
    group
        [ rect 100 75
            |> outlined (solid (rgb 60 100 60))
            |> move barrel.location
            |> rotate barrel.angle
        , center barrel.location
        , collisionCircle barrel.location barrel.collisionRadius
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
