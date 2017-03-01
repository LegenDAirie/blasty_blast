module Barrel exposing (renderBarrel, updateBarrels, shouldBarrelFire, nearestPiOverFour)

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
        , movement = updateMovementSpec deltaTime barrel.movement
    }
        |> updateRotation deltaTime controls


inactiveRotationSpec : Rotation -> Rotation
inactiveRotationSpec rotation =
    case rotation of
        NoRotation { fireType } ->
            case fireType of
                AutoFire ->
                    NoRotation (NoRotationSpec AutoFire)

                ManualFire setToFire ->
                    NoRotation (NoRotationSpec (ManualFire False))

        AutoRotateToAndStop { fireType, endAngle } ->
            case fireType of
                AutoFire ->
                    AutoRotateToAndStop (AutoRotateToAndStopSpec AutoFire endAngle)

                ManualFire setToFire ->
                    AutoRotateToAndStop (AutoRotateToAndStopSpec (ManualFire False) endAngle)

        AutoWithNoControl spec ->
            { spec
                | setToFire = False
            }
                |> AutoWithNoControl

        AutoWithDirectionControl spec ->
            { spec
                | setToFire = False
            }
                |> AutoWithDirectionControl

        ManualRotation spec ->
            { spec
                | setToFire = False
            }
                |> ManualRotation

        ManualTimedFire spec ->
            { spec
                | setToFire = False
            }
                |> ManualTimedFire


updateRotation : DeltaTime -> PlayerControls -> Barrel -> Barrel
updateRotation deltaTime controls barrel =
    case barrel.rotation of
        NoRotation { fireType } ->
            let
                newRotation =
                    case fireType of
                        AutoFire ->
                            barrel.rotation

                        ManualFire setToFire ->
                            case setToFire of
                                True ->
                                    barrel.rotation

                                False ->
                                    if controls.fire == Pressed then
                                        NoRotation (NoRotationSpec (ManualFire True))
                                    else
                                        barrel.rotation
            in
                { barrel
                    | rotation = newRotation
                }

        AutoRotateToAndStop { fireType, endAngle } ->
            let
                newRotation =
                    case fireType of
                        AutoFire ->
                            barrel.rotation

                        ManualFire setToFire ->
                            case setToFire of
                                True ->
                                    barrel.rotation

                                False ->
                                    if controls.fire == Pressed then
                                        AutoRotateToAndStop (AutoRotateToAndStopSpec (ManualFire True) endAngle)
                                    else
                                        barrel.rotation

                updatedAngle =
                    if barrel.angle < endAngle then
                        clamp barrel.angle endAngle (barrel.angle + deltaTime * 5)
                    else
                        clamp endAngle barrel.angle (barrel.angle - deltaTime * 5)
            in
                { barrel
                    | rotation = newRotation
                    , angle = updatedAngle
                }

        AutoWithNoControl { setToFire, range, clockWise, rotationStyle } ->
            let
                newRotation =
                    case setToFire of
                        True ->
                            barrel.rotation

                        False ->
                            if controls.fire == Pressed then
                                AutoWithNoControl (AutoWithNoControlSpec True range clockWise rotationStyle)
                            else
                                barrel.rotation

                newAngle =
                    if clockWise then
                        barrel.angle - deltaTime * 5
                    else
                        barrel.angle + deltaTime * 5

                setAngle =
                    if controls.fire == Pressed && exceededMinTimeOccupiedToFire barrel.timeOccupied then
                        nearestPiOverFour newAngle
                    else
                        newAngle
            in
                { barrel
                    | rotation = newRotation
                    , angle = setAngle
                }

        AutoWithDirectionControl { setToFire, clockWise } ->
            let
                readyToFire =
                    case setToFire of
                        True ->
                            setToFire

                        False ->
                            if controls.fire == Pressed then
                                True
                            else
                                setToFire

                stillClockWise =
                    if controls.left == Pressed || controls.left == Held then
                        False
                    else if controls.right == Pressed || controls.right == Held then
                        True
                    else
                        clockWise

                newRotation =
                    AutoWithDirectionControl (AutoWithDirectionControlSpec readyToFire stillClockWise)

                newAngle =
                    if stillClockWise then
                        barrel.angle - deltaTime * 5
                    else
                        barrel.angle + deltaTime * 5

                setAngle =
                    if controls.fire == Pressed && exceededMinTimeOccupiedToFire barrel.timeOccupied then
                        nearestPiOverFour newAngle
                    else
                        newAngle
            in
                { barrel
                    | rotation = newRotation
                    , angle = newAngle
                }

        ManualRotation { setToFire, range } ->
            let
                newRotation =
                    case setToFire of
                        True ->
                            barrel.rotation

                        False ->
                            if controls.fire == Pressed then
                                ManualRotation (ManualRotationSpec True range)
                            else
                                barrel.rotation

                ( minAngle, maxAngle ) =
                    range

                newAngle =
                    if controls.left == Pressed || controls.left == Held then
                        clamp minAngle maxAngle barrel.angle - deltaTime * 5
                    else if controls.right == Pressed || controls.right == Held then
                        clamp minAngle maxAngle barrel.angle + deltaTime * 5
                    else
                        barrel.angle

                setAngle =
                    if controls.fire == Pressed && exceededMinTimeOccupiedToFire barrel.timeOccupied then
                        nearestPiOverFour newAngle
                    else
                        newAngle
            in
                { barrel
                    | rotation = newRotation
                    , angle = setAngle
                }

        ManualTimedFire { setToFire, maxTimeOccupied } ->
            let
                newRotation =
                    case setToFire of
                        True ->
                            barrel.rotation

                        False ->
                            if barrel.timeOccupied >= maxTimeOccupied then
                                ManualTimedFire (ManualTimedFireSpec True maxTimeOccupied)
                            else
                                barrel.rotation

                newAngle =
                    if controls.left == Pressed || controls.left == Held then
                        barrel.angle + deltaTime * 5
                    else if controls.right == Pressed || controls.right == Held then
                        barrel.angle - deltaTime * 5
                    else
                        barrel.angle

                setAngle =
                    if controls.fire == Pressed && exceededMinTimeOccupiedToFire barrel.timeOccupied then
                        nearestPiOverFour newAngle
                    else
                        newAngle
            in
                { barrel
                    | rotation = newRotation
                    , angle = setAngle
                }


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

        ManualTimedFire { setToFire } ->
            setToFire && exceededMinTimeOccupiedToFire barrel.timeOccupied

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


nearestPiOverFour : Float -> Float
nearestPiOverFour angle =
    angle
        |> (\number -> number / (pi / 4))
        |> round
        |> (\number -> toFloat number * (pi / 4))



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
