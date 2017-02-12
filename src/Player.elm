module Player exposing (updatePlayer, renderPlayer, PlayerControls, initialPlayerControls)

import Vector2 as V2
import Game.TwoD.Render as Render exposing (Renderable, rectangle)
import Game.Resources as Resources exposing (Resources)
import Color
import GameTypes exposing (ActiveElement(..), Vector, DeltaTime, Player, Barrel)
import Forces exposing (gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)
import Button exposing (ButtonState(..), calculateButtonState)


type alias PlayerControls =
    { left : ButtonState
    , right : ButtonState
    , fire : ButtonState
    }


initialPlayerControls : PlayerControls
initialPlayerControls =
    { left = Inactive
    , right = Inactive
    , fire = Inactive
    }


calculateButtonsPressed : List Vector -> PlayerControls -> PlayerControls
calculateButtonsPressed touchLocations playerControls =
    { left = calculateButtonState (List.any (\( x, y ) -> x < 320) touchLocations) playerControls.left
    , right = calculateButtonState (List.any (\( x, y ) -> x > 960) touchLocations) playerControls.right
    , fire = calculateButtonState (List.any (\( x, y ) -> x > 320 && x < 960) touchLocations) playerControls.fire
    }


updatePlayer : DeltaTime -> ActiveElement -> List Vector -> PlayerControls -> Player -> ( ActiveElement, Player )
updatePlayer deltaTime activeElement touchLocations playerControls player =
    let
        buttonsPressed =
            calculateButtonsPressed touchLocations playerControls

        newPlayer =
            applyPhysics deltaTime activeElement buttonsPressed player

        ( newActiveElement, firedPlayer ) =
            if playerControls.fire == Pressed then
                ( ThePlayer, fire activeElement newPlayer )
            else
                ( activeElement, newPlayer )
    in
        ( newActiveElement, firedPlayer )


applyPhysics : DeltaTime -> ActiveElement -> PlayerControls -> Player -> Player
applyPhysics deltaTime activeElement playerControls player =
    let
        gravitationalForce =
            V2.scale deltaTime gravity

        currentControllerForce =
            V2.scale deltaTime <|
                if playerControls.left == Pressed || playerControls.left == Held then
                    controllerLeftForce
                else if playerControls.right == Pressed || playerControls.right == Held then
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
