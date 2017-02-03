module Controls exposing (PlayTestButton(..), calculatePlayTestControls)

import GameTypes exposing (Vector)


-- type GameControls
--     = PlayTestControls
--     | LevelEditControls
--
--
-- type LevelEditControls
--     = AddBarrel
--     | PlayTestMode
--
--
-- type PlayTestControls
--     = DPad Direction
--     | Fire
--     | EditMode
--
--
-- type Direction
--     = Left
--     | Right


type PlayTestButton
    = Left
    | Right
    | Fire



-- calculateButtonsPressed : List Vector -> List PlayTestButton
-- calculateButtonsPressed touchLocations =
--     List.filterMap convertTouchToButton touchLocations
-- convertTouchToButton : Vector -> Maybe PlayTestButton
-- convertTouchToButton ( touchX, touchY ) =
--     if touchX < 320 then
--         Just Left
--     else if touchX > 320 && touchX < 960 then
--         Just Fire
--     else if touchX > 960 then
--         Just Left
--     else
--         Nothing
-- case gameScreen of
--     PlayTest ->
--         calculatePlayTestControls touchLocation
--
--     LevelEdit ->
--         calculatePlayTestControls touchLocation


calculatePlayTestControls : List Vector -> List PlayTestButton
calculatePlayTestControls touchLocations =
    List.filterMap calculatePlayTestControl touchLocations


calculatePlayTestControl : Vector -> Maybe PlayTestButton
calculatePlayTestControl ( touchX, touchY ) =
    if touchX < 320 then
        Just Left
    else if touchX > 320 && touchX < 960 then
        Just Fire
    else if touchX > 960 then
        Just Right
    else
        Nothing
