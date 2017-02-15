module GameLogic exposing (calculateActivePlayElement, repostionBarrels)

import Vector2 as V2 exposing (distance)
import GameTypes exposing (Vector, Player, Barrel, ActiveElement(..))


calculateActivePlayElement : Player -> List Barrel -> ActiveElement
calculateActivePlayElement player barrels =
    case barrels of
        barrel :: rest ->
            if hasPlayerCollided player barrel then
                ThisBarrel barrel
            else
                calculateActivePlayElement player rest

        [] ->
            ThePlayer


hasPlayerCollided : Player -> Barrel -> Bool
hasPlayerCollided player barrel =
    let
        distanceBetween =
            distance player.location barrel.location

        collectiveRadius =
            player.collisionRadius + barrel.collisionRadius
    in
        distanceBetween < toFloat collectiveRadius


repostionBarrels : List Vector -> List Barrel -> List Barrel
repostionBarrels touchLocations barrels =
    case touchLocations of
        touch :: rest ->
            let
                moveToTouchLocation =
                    repositionBarrel touch
            in
                List.map moveToTouchLocation barrels
                    |> repostionBarrels rest

        [] ->
            barrels


repositionBarrel : Vector -> Barrel -> Barrel
repositionBarrel touchLocation barrel =
    let
        distanceBetween =
            distance touchLocation barrel.location
    in
        if distanceBetween < toFloat barrel.collisionRadius then
            { barrel
                | location = touchLocation
            }
        else
            barrel
