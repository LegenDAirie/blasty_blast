module Controls exposing (calculateButtonsPressed)

import GameTypes exposing (Vector, CreateMode(..), PlayTestControls(..))


calculateButtonsPressed : CreateMode -> List Vector -> List PlayTestControls
calculateButtonsPressed mode touchLocations =
    let
        buttonPressed =
            case mode of
                PlayTest ->
                    calculatePlayTestControls touchLocations

                Edit ->
                    calculatePlayTestControls touchLocations
    in
        [ buttonPressed ]


calculatePlayTestControls : List Vector -> PlayTestControls
calculatePlayTestControls touchLocations =
    case touchLocations of
        ( x, y ) :: rest ->
            if x < 320 then
                Left
            else if x >= 320 && x < 960 then
                Fire
            else if x > 960 then
                Right
            else
                None

        _ ->
            None
