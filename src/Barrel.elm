module Barrel exposing (updateBarrel, renderBarrel)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (Vector, Barrel)


type alias Angle =
    Float


updateBarrel : (Barrel -> Barrel) -> List Barrel -> Barrel -> List Barrel
updateBarrel fn barrels barrelToUpdate =
    case barrels of
        barrel :: rest ->
            if barrel == barrelToUpdate then
                fn barrel :: rest
            else
                barrel :: updateBarrel fn rest barrelToUpdate

        [] ->
            []



-- rotation
-- let
--     transformBarrel =
--         Barrel.rotate (pi / 4)
-- in
--     { model
--         | barrels = updateBarrel transformBarrel model.barrels barrel
--     }


rotate : Angle -> (Barrel -> Barrel)
rotate offsetAngle barrel =
    { barrel
        | angle = barrel.angle + offsetAngle
    }


renderBarrel : Barrel -> Renderable
renderBarrel barrel =
    let
        x =
            getX barrel.location - toFloat barrel.collisionRadius

        y =
            getY barrel.location - toFloat barrel.collisionRadius
    in
        Render.rectangleWithOptions
            { color = Color.brown
            , position = ( x, y, 0 )
            , rotation = barrel.angle
            , size = ( toFloat barrel.collisionRadius * 2, toFloat barrel.collisionRadius * 2 )
            , pivot = ( 0.5, 0.5 )
            }
