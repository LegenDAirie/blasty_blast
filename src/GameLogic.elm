module GameLogic exposing (calculateActiveElement)

import Vector2 as V2 exposing (distance)
import GameTypes exposing (Player, Barrel, ActiveElement(..))


calculateActiveElement : Player -> List Barrel -> ActiveElement
calculateActiveElement player barrels =
    case barrels of
        barrel :: rest ->
            if hasCollided player barrel then
                ThisBarrel barrel
            else
                calculateActiveElement player rest

        [] ->
            ThePlayer


hasCollided : Player -> Barrel -> Bool
hasCollided player barrel =
    let
        distanceBetween =
            distance player.location barrel.location

        collectiveRadius =
            player.collisionRadius + barrel.collisionRadius
    in
        distanceBetween < toFloat collectiveRadius
