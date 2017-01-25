module Forces exposing (moveLeft, moveRight, dontMove)

import GameTypes exposing (ActiveElement(..), Controles(..), Force(..), Barrel, Model)
import Barrel exposing (updateBarrel)


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
