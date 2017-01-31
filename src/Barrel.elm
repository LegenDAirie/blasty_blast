module Barrel exposing (updateBarrel, rotate)

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


rotate : Angle -> (Barrel -> Barrel)
rotate offsetAngle barrel =
    { barrel
        | angle = barrel.angle + offsetAngle
    }
