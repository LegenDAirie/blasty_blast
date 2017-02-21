module GameLogic
    exposing
        ( calculateActivePlayElement
        , repostionBarrels
        , removeOverlappingBarrels
        , touchIsCollidingWithBarrel
        , hasPlayerCollided
        , areAnyBarrelsInTheWay
        , anyBarrelsTouched
        )

import Vector2 as V2 exposing (distance)
import GameTypes exposing (Vector, Player, Barrel, ActiveElement(..))
import Set


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


areAnyBarrelsInTheWay : List Vector -> List Barrel -> Bool
areAnyBarrelsInTheWay touchLocations barrels =
    List.any (anyBarrelsTouched barrels) touchLocations


anyBarrelsTouched : List Barrel -> Vector -> Bool
anyBarrelsTouched barrels touch =
    List.any (touchIsCollidingWithBarrel touch) barrels


touchIsCollidingWithBarrel : Vector -> Barrel -> Bool
touchIsCollidingWithBarrel touchLocation barrel =
    let
        distanceBetween =
            distance touchLocation barrel.location
    in
        distanceBetween < toFloat barrel.collisionRadius


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


removeOverlappingBarrels : List Barrel -> List Barrel
removeOverlappingBarrels list =
    let
        step next ( set, acc ) =
            if Set.member next.location set then
                ( set, acc )
            else
                ( Set.insert next.location set, next :: acc )
    in
        List.foldl step ( Set.empty, [] ) list
            |> (\( first, second ) -> second)
            |> List.reverse
