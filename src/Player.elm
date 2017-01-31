module Player exposing (updatePlayer, fireFromBarrel)

import Vector2 as V2
import GameTypes exposing (ActiveElement(..), Player, Barrel)
import Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)


type alias Vector =
    ( Float, Float )


type alias DeltaTime =
    Float


updatePlayer : DeltaTime -> ActiveElement -> Player -> Player
updatePlayer dt activeElement player =
    let
        gravitationalForce =
            V2.scale dt gravity

        -- currentControllerForce =
        --     V2.scale dt <|
        --         case moveDirection of
        --             GoLeft ->
        --                 controllerLeftForce
        --
        --             GoRight ->
        --                 controllerRightForce
        --
        --             GoWithTheFlow ->
        --                 ( 0, 0 )
        newVelocity =
            player.velocity
                |> V2.add gravitationalForce
                -- |> V2.add currentControllerForce
                |>
                    (\( x, y ) -> ( x * resistance, y ))
                |> capHorizontalVelocity speedCap
                |> capVerticalVelocity speedCap

        newLocation =
            newVelocity
                |> V2.add player.location
                |> resetPlayerToOrigin
    in
        case activeElement of
            ThePlayer ->
                { player
                    | location = newLocation
                    , velocity = newVelocity
                }

            ThisBarrel barrel ->
                { player
                    | location = barrel.location
                    , velocity = ( 0, 0 )
                }


resetPlayerToOrigin : Vector -> Vector
resetPlayerToOrigin location =
    if V2.getY location < -1000 then
        ( 0, 0 )
    else
        location


fire : ActiveElement -> Player -> Player
fire activeElement player =
    case activeElement of
        ThePlayer ->
            player

        ThisBarrel barrel ->
            fireFromBarrel barrel player


fireFromBarrel : Barrel -> Player -> Player
fireFromBarrel barrel player =
    let
        minDistanceApart =
            toFloat (barrel.collisionRadius + player.collisionRadius)

        directionVector =
            ( cos barrel.angle, sin barrel.angle )

        newLocatioin =
            directionVector
                |> V2.scale minDistanceApart
                |> V2.add barrel.location

        newVelocity =
            directionVector
                |> V2.scale blastForce
    in
        { player
            | location = newLocatioin
            , velocity = newVelocity
        }


capHorizontalVelocity : Float -> Vector -> Vector
capHorizontalVelocity maxSpeed ( x, y ) =
    if x > maxSpeed then
        ( maxSpeed, y )
    else if x < -maxSpeed then
        ( -maxSpeed, y )
    else
        ( x, y )


capVerticalVelocity : Float -> Vector -> Vector
capVerticalVelocity maxSpeed ( x, y ) =
    if y < -maxSpeed then
        ( x, -maxSpeed )
    else
        ( x, y )
