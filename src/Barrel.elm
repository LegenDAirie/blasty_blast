module Barrel exposing (renderBarrel, updateBarrels, shouldBarrelFire)

import Game.TwoD.Render as Render exposing (Renderable)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (..)
import Button exposing (ButtonState(..))


updateBarrels : DeltaTime -> ActiveElement -> PlayerControls -> List Barrel -> List Barrel
updateBarrels dt activeElement controls barrels =
    barrels
        |> List.map (updateBarrel dt activeElement controls)


updateBarrel : DeltaTime -> ActiveElement -> PlayerControls -> Barrel -> Barrel
updateBarrel deltaTime activeElement controls barrel =
    case activeElement of
        ThePlayer ->
            updateNonActiveBarrel deltaTime barrel

        ThisBarrel activeBarrel ->
            if activeBarrel == barrel then
                updateActiveBarrel deltaTime controls barrel
            else
                updateNonActiveBarrel deltaTime barrel


updateNonActiveBarrel : DeltaTime -> Barrel -> Barrel
updateNonActiveBarrel deltaTime barrel =
    { barrel
        | timeOccupied = 0
        , rotation = inactiveRotationSpec barrel.rotation
        , movement = updateMovementSpec deltaTime barrel.movement
    }


updateActiveBarrel : DeltaTime -> PlayerControls -> Barrel -> Barrel
updateActiveBarrel deltaTime controls barrel =
    { barrel
        | timeOccupied = barrel.timeOccupied + deltaTime
        , rotation = updateRotationSpec deltaTime controls barrel.rotation
        , movement = updateMovementSpec deltaTime barrel.movement
    }


inactiveRotationSpec : Rotation -> Rotation
inactiveRotationSpec rotation =
    case rotation of
        NoRotation { fireType } ->
            case fireType of
                AutoFire ->
                    NoRotation (NoRotationSpec AutoFire)

                ManualFire setToFire ->
                    NoRotation (NoRotationSpec (ManualFire False))

        AutoWithNoControl { setToFire } ->
            rotation

        AutoWithDirectionControl { setToFire } ->
            rotation

        AutoRotateToAndStop { fireType } ->
            rotation

        ManualRotation { setToFire } ->
            rotation

        ManualTimedFire { maxTimeOccupied } ->
            rotation


updateRotationSpec : DeltaTime -> PlayerControls -> Rotation -> Rotation
updateRotationSpec dt controls rotation =
    case rotation of
        NoRotation { fireType } ->
            case fireType of
                AutoFire ->
                    rotation

                ManualFire setToFire ->
                    case setToFire of
                        True ->
                            rotation

                        False ->
                            if controls.fire == Pressed then
                                NoRotation (NoRotationSpec (ManualFire True))
                            else
                                rotation

        AutoWithNoControl { setToFire, range, clockWise, rotationStyle } ->
            rotation

        AutoWithDirectionControl { clockWise } ->
            rotation

        AutoRotateToAndStop { fireType, endAngle } ->
            rotation

        ManualRotation { range } ->
            rotation

        ManualTimedFire { maxTimeOccupied } ->
            rotation


updateMovementSpec : DeltaTime -> Movement -> Movement
updateMovementSpec deltaTime movement =
    case movement of
        NoMovement ->
            movement

        LinePath { startNode, endNode, startingDirection, speed } ->
            movement

        CirclePath { trackRadius, clockWiseMovement } ->
            movement


shouldBarrelFire : Barrel -> Bool
shouldBarrelFire barrel =
    case barrel.rotation of
        NoRotation { fireType } ->
            case fireType of
                AutoFire ->
                    exceededMinTimeOccupiedToFire barrel.timeOccupied

                ManualFire setToFire ->
                    setToFire && exceededMinTimeOccupiedToFire barrel.timeOccupied

        AutoRotateToAndStop { fireType, endAngle } ->
            case fireType of
                AutoFire ->
                    exceededMinTimeOccupiedToFire barrel.timeOccupied

                ManualFire setToFire ->
                    setToFire && exceededMinTimeOccupiedToFire barrel.timeOccupied

        ManualTimedFire { maxTimeOccupied } ->
            if maxTimeOccupied <= 0 then
                True
            else
                False

        AutoWithNoControl { setToFire } ->
            setToFire && exceededMinTimeOccupiedToFire barrel.timeOccupied

        AutoWithDirectionControl { setToFire } ->
            setToFire && exceededMinTimeOccupiedToFire barrel.timeOccupied

        ManualRotation { setToFire } ->
            setToFire && exceededMinTimeOccupiedToFire barrel.timeOccupied


exceededMinTimeOccupiedToFire : Float -> Bool
exceededMinTimeOccupiedToFire timeOccupied =
    if timeOccupied >= 0.5 then
        True
    else
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
-- rotate : Float -> (Barrel -> Barrel)
-- rotate offsetAngle barrel =
--     { barrel
--         | angle = barrel.angle + offsetAngle
--     }


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
