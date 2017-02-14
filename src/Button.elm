module Button exposing (ButtonState(..), calculateButtonState)


type ButtonState
    = Pressed
    | Held
    | Released
    | Inactive


calculateButtonState : Bool -> ButtonState -> ButtonState
calculateButtonState isTouched currentButtonState =
    case isTouched of
        True ->
            case currentButtonState of
                Pressed ->
                    Held

                Held ->
                    Held

                Released ->
                    Pressed

                Inactive ->
                    Pressed

        False ->
            case currentButtonState of
                Pressed ->
                    Released

                Held ->
                    Released

                Released ->
                    Inactive

                Inactive ->
                    Inactive
