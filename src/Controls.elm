module Controls exposing (PlayTestControls(..), calculateButtonsPressed)

import GameTypes exposing (Vector, GameScreens(..))


type GameControls
    = PlayTestControls
    | LevelEditControls


type LevelEditControls
    = AddBarrel
    | PlayTestMode


type PlayTestControls
    = DPad Direction
    | Fire
    | EditMode


type Direction
    = Left
    | Right


calculateButtonsPressed : GameScreens -> List Vector -> List GameControls
calculateButtonsPressed gameScreen touchLocations =
    List.filterMap (convertTouchToButton gameScreen) touchLocations



-- |> List.filterMap (\item -> item /= Nothing)


convertTouchToButton : GameScreens -> Vector -> Maybe GameControls
convertTouchToButton gameScreen touchLocation =
    Just PlayTestControls



-- case gameScreen of
--     PlayTest ->
--         calculatePlayTestControls touchLocation
--
--     LevelEdit ->
--         calculatePlayTestControls touchLocation


calculatePlayTestControls : Vector -> Maybe PlayTestControls
calculatePlayTestControls ( x, y ) =
    Just Fire



-- if x < 320 then
--     Just (DPad Left)
-- else if x >= 320 && x < 960 then
--     Just Fire
-- else if x > 960 then
--     Just (DPad Right)
-- else
--     Nothing
