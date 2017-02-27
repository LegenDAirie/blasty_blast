module Barrel exposing (renderBarrel, updateBarrels, shouldBarrelFire)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (..)


updateBarrels : DeltaTime -> ActiveElement -> PlayerControls -> List Barrel -> List Barrel
updateBarrels dt activeElement controls barrels =
    barrels
        |> List.map (updateBarrel dt activeElement controls)


updateBarrel : DeltaTime -> ActiveElement -> PlayerControls -> Barrel -> Barrel
updateBarrel dt activeElement controls barrel =
    barrel
        |> (updateRotation dt activeElement controls)
        |> (updateMovement dt activeElement)


updateRotation : DeltaTime -> ActiveElement -> PlayerControls -> Barrel -> Barrel
updateRotation dt activeElement controls barrel =
    case barrel.rotation of
        NoRotation { fireType } ->
            barrel

        AutoWithNoControl { range, clockWise, rotationStyle } ->
            barrel

        AutoWithDirectionControl { clockWise } ->
            barrel

        AutoRotateToAndStop { fireType, endAngle } ->
            barrel

        ManualRotation { range } ->
            barrel

        ManualTimedFire { currentCountDown, delay } ->
            barrel


updateMovement : DeltaTime -> ActiveElement -> Barrel -> Barrel
updateMovement dt activeElement barrel =
    case barrel.movement of
        NoMovement ->
            barrel

        LinePath { startNode, endNode, startingDirection, speed } ->
            barrel

        CirclePath { trackRadius, clockWiseMovement } ->
            barrel


shouldBarrelFire : Barrel -> Bool
shouldBarrelFire barrel =
    case barrel.rotation of
        NoRotation { fireType } ->
            case fireType of
                AutoFire countDownTimer ->
                    if countDownTimer <= 0 then
                        True
                    else
                        False

                ManualFire ->
                    False

        AutoRotateToAndStop { fireType, endAngle } ->
            case fireType of
                AutoFire countDownTimer ->
                    if countDownTimer <= 0 then
                        True
                    else
                        False

                ManualFire ->
                    False

        ManualTimedFire { currentCountDown, delay } ->
            if currentCountDown <= 0 then
                True
            else
                False

        AutoWithNoControl spec ->
            False

        AutoWithDirectionControl spec ->
            False

        ManualRotation spec ->
            False



-- updateBarrel : (Barrel -> Barrel) -> List Barrel -> Barrel -> List Barrel
-- updateBarrel fn barrels barrelToUpdate =
--     case barrels of
--         barrel :: rest ->
--             if barrel == barrelToUpdate then
--                 fn barrel :: rest
--             else
--                 barrel :: updateBarrel fn rest barrelToUpdate
--
--         [] ->
--             []
-- rotation
-- let
--     transformBarrel =
--         Barrel.rotate (pi / 4)
-- in
--     { model
--         | barrels = updateBarrel transformBarrel model.barrels barrel
--     }


rotate : Float -> (Barrel -> Barrel)
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
