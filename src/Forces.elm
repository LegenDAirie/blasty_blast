module Forces exposing (moveLeft, moveRight, dontMove, gravity, controllerLeftForce, controllerRightForce, speedCap, resistance, blastForce)

import GameTypes exposing (ActiveElement(..), Controles(..), Force(..), Barrel, Model, Vector)
import Barrel exposing (updateBarrel)


speedCap =
    10


blastForce =
    20


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


moveLeft : Model -> Model
moveLeft model =
    case model.active of
        ThePlayer ->
            { model
                | force = GoLeft
            }

        ThisBarrel barrel ->
            let
                transformBarrel =
                    Barrel.rotate (pi / 4)
            in
                { model
                    | barrels = updateBarrel transformBarrel model.barrels barrel
                }


moveRight : Model -> Model
moveRight model =
    case model.active of
        ThePlayer ->
            { model
                | force = GoRight
            }

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
    { model
        | force = GoWithTheFlow
    }
