module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App exposing (..)
import GameTypes exposing (Player, Barrel)
import GameLogic exposing (hasPlayerCollided, touchIsCollidingWithBarrel)
import Screens.LevelCreationScreen exposing (areAnyBarrelsInTheWay)


all : Test
all =
    describe "Game"
        [ describe "A Test Suite"
            [ test "Addition" <|
                \() ->
                    Expect.equal (3 + 7) 10
            , test "String.left" <|
                \() ->
                    Expect.equal "a" (String.left 1 "abcdefg")
            ]
        , describe "collision detection"
            [ test "collision" <|
                \() ->
                    let
                        player =
                            Player ( 2, 3 ) ( 0, 0 ) 1

                        barrel =
                            Barrel ( 3, 2 ) (pi / 4) 1
                    in
                        Expect.true "Expected collision" (hasPlayerCollided player barrel)
            , test "collision" <|
                \() ->
                    let
                        player =
                            Player ( 7, 4 ) ( 0, 0 ) 1

                        barrel =
                            Barrel ( 9, 3 ) (pi / 4) 1
                    in
                        Expect.false "Expected no collision" (hasPlayerCollided player barrel)
            ]
        , describe "Game Logic Helper Functions"
            [ test "touch colliding with barrel" <|
                \() ->
                    touchIsCollidingWithBarrel ( 3, 2 ) (Barrel ( 3, 2 ) 0 2)
                        |> Expect.true "Expected a barrel in the way"
            , test "touchs colliding with barrels" <|
                \() ->
                    let
                        touches =
                            [ ( 3, 2 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 4, 2 ) 0 2

                        barrelTwo =
                            Barrel ( 0, 0 ) 0 2

                        barrels =
                            [ barrelOne, barrelTwo ]
                    in
                        Expect.true "Expected barrels in the way" (areAnyBarrelsInTheWay touches barrels)
            , test "no collisions" <|
                \() ->
                    let
                        touches =
                            [ ( 4, 7 ), ( 7, 4 ) ]

                        barrelOne =
                            Barrel ( 20, 20 ) 0 2

                        barrelTwo =
                            Barrel ( 30, 30 ) 0 2

                        barrels =
                            [ barrelOne, barrelTwo ]
                    in
                        Expect.false "Expected no barrels in the way" (areAnyBarrelsInTheWay touches barrels)
            ]
        ]
