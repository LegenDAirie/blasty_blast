module Player exposing (updatePlayer, fireFromBarrel)

import Vector2 as V2
import GameTypes exposing (ActiveElement(..), Vector, DeltaTime, Player, Barrel)
import Controls exposing (PlayTestButton(..))
import Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)


updatePlayer : DeltaTime -> ActiveElement -> List PlayTestButton -> Player -> ( ActiveElement, Player )
updatePlayer deltaTime activeElement playTestButtons player =
    let
        newPlayer =
            applyPhysics deltaTime activeElement playTestButtons player

        ( newActiveElement, firedPlayer ) =
            if List.member Fire playTestButtons then
                ( ThePlayer, fire activeElement newPlayer )
            else
                ( activeElement, newPlayer )
    in
        ( newActiveElement, firedPlayer )


applyPhysics : DeltaTime -> ActiveElement -> List PlayTestButton -> Player -> Player
applyPhysics deltaTime activeElement playTestButtons player =
    let
        gravitationalForce =
            V2.scale deltaTime gravity

        currentControllerForce =
            V2.scale deltaTime <|
                if List.member Left playTestButtons then
                    controllerLeftForce
                else if List.member Right playTestButtons then
                    controllerRightForce
                else
                    ( 0, 0 )

        newVelocity =
            player.velocity
                |> V2.add gravitationalForce
                |> V2.add currentControllerForce
                |> (\( x, y ) -> ( x * resistance, y ))
                |> capHorizontalVelocity speedCap
                |> capVerticalVelocity speedCap

        newLocation =
            newVelocity
                |> V2.add player.location
                |> resetPlayerToOrigin

        newPlayer =
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
    in
        newPlayer


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
