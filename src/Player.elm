module Player exposing (updatePlayer)

import Vector2 as V2
import GameTypes exposing (Force(..), ActiveElement(..), Player)


type alias Vector =
    ( Float, Float )


type alias DeltaTime =
    Float


updatePlayer : DeltaTime -> ActiveElement -> Player -> Force -> Player
updatePlayer dt activeElement player moveDirection =
    let
        gravity =
            V2.scale dt ( 0, -1 )

        moveForce =
            V2.scale dt <|
                case moveDirection of
                    GoLeft ->
                        ( -1, 0 )

                    GoRight ->
                        ( 1, 0 )

                    GoWithTheFlow ->
                        ( 0, 0 )

        newVelocity =
            player.velocity
                |> V2.add gravity
                |> V2.add moveForce
                |> capHorizontalVelocity 10
                |> capVerticalVelocity 10
    in
        case activeElement of
            ThePlayer ->
                { player
                    | location = V2.add player.location newVelocity
                    , velocity = newVelocity
                }

            ThisBarrel barrel ->
                { player
                    | location = barrel.location
                    , velocity = ( 0, 0 )
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
