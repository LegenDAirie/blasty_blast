module Tests exposing (..)

import Test exposing (..)
import Expect
import String
import App exposing (Player, Barrel, hasCollided)


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
                        Expect.true "Expected collision" (hasCollided player barrel)
            , test "collision" <|
                \() ->
                    let
                        player =
                            Player ( 7, 4 ) ( 0, 0 ) 1

                        barrel =
                            Barrel ( 9, 3 ) (pi / 4) 1
                    in
                        Expect.false "Expected no collision" (hasCollided player barrel)
            ]
        ]
