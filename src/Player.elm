module Player exposing (updatePlayer, renderPlayer)

import Vector2 as V2
import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Color
import GameTypes exposing (ActiveElement(..), Vector, DeltaTime, Player, Barrel)
import Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)


type PlayerControles
    = Left
    | Right
    | Fire


calculateButtonsPressed : List Vector -> List PlayerControles
calculateButtonsPressed touchLocations =
    List.filterMap calculateButtonPressed touchLocations


calculateButtonPressed : Vector -> Maybe PlayerControles
calculateButtonPressed ( touchX, touchY ) =
    if touchX < 320 then
        Just Left
    else if touchX > 320 && touchX < 960 then
        Just Fire
    else if touchX > 960 then
        Just Right
    else
        Nothing


updatePlayer : DeltaTime -> ActiveElement -> List Vector -> Player -> ( ActiveElement, Player )
updatePlayer deltaTime activeElement touchLocations player =
    let
        buttonsPressed =
            calculateButtonsPressed touchLocations

        newPlayer =
            applyPhysics deltaTime activeElement buttonsPressed player

        ( newActiveElement, firedPlayer ) =
            if List.member Fire buttonsPressed then
                ( ThePlayer, fire activeElement newPlayer )
            else
                ( activeElement, newPlayer )
    in
        ( newActiveElement, firedPlayer )


applyPhysics : DeltaTime -> ActiveElement -> List PlayerControles -> Player -> Player
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


renderPlayer : Resources -> Player -> Renderable
renderPlayer resources player =
    -- let
    --     ( x, y ) =
    --         player.location
    -- in
    --     Render.animatedSpriteWithOptions
    --         { position = ( x, y, 0 )
    --         , size = ( toFloat player.collisionRadius * 2, toFloat player.collisionRadius * 2 )
    --         , texture = Resources.getTexture "../assets/ghost-friend.png" resources
    --         , bottomLeft = ( 0, 0 )
    --         , topRight = ( 1, 1 )
    --         , duration = 1
    --         , numberOfFrames = 8
    --         , rotation = 0
    --         , pivot = ( 0.5, 0 )
    --         }
    Render.rectangle
        { color = Color.charcoal
        , position = player.location
        , size = ( toFloat player.collisionRadius * 2, toFloat player.collisionRadius * 2 )
        }
