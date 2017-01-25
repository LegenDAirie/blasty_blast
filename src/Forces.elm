module Forces exposing (moveLeft, moveRight, dontMove, gravity, controllerLeftForce, controllerRightForce, blastForce)

import GameTypes exposing (ActiveElement(..), Controles(..), Force(..), Barrel, Model, Vector)
import Barrel exposing (updateBarrel)


gravity : Vector
gravity =
    ( 0, -50 )


blastForce : Float
blastForce =
    20


controllerLeftForce : Vector
controllerLeftForce =
    ( -20, 0 )


controllerRightForce : Vector
controllerRightForce =
    ( 20, 0 )


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
