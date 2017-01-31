module Forces exposing (moveLeft, moveRight, dontMove, gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)

import GameTypes exposing (ActiveElement(..), PlayTestControls(..), Barrel, Model, Vector)
import Barrel exposing (updateBarrel)


speedCap : Float
speedCap =
    10


blastForce : Float
blastForce =
    20


resistance : Float
resistance =
    0.98


gravity : Vector
gravity =
    ( 0, -30 )


controllerLeftForce : Vector
controllerLeftForce =
    ( -50, 0 )


controllerRightForce : Vector
controllerRightForce =
    ( 50, 0 )


moveLeft : ActiveElement -> Model -> Model
moveLeft activeElement model =
    case activeElement of
        ThePlayer ->
            model

        ThisBarrel barrel ->
            let
                transformBarrel =
                    Barrel.rotate (pi / 4)
            in
                { model
                    | barrels = updateBarrel transformBarrel model.barrels barrel
                }


moveRight : ActiveElement -> Model -> Model
moveRight activeElement model =
    case activeElement of
        ThePlayer ->
            model

        ThisBarrel barrel ->
            let
                transformBarrel =
                    Barrel.rotate (-pi / 4)
            in
                { model
                    | barrels = updateBarrel transformBarrel model.barrels barrel
                }


dontMove : Model -> Model
dontMove model =
    model
