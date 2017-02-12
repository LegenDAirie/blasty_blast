module Button exposing (ButtonState(..), calculateButtonState)


type ButtonState
    = Pressed
    | Held
    | Released
    | Inactive


calculateButtonState : Bool -> ButtonState -> ButtonState
calculateButtonState isTouched buttonState =
    case isTouched of
        True ->
            case buttonState of
                Pressed ->
                    Held

                Held ->
                    Held

                Released ->
                    Pressed

                Inactive ->
                    Pressed

        False ->
            case buttonState of
                Pressed ->
                    Released

                Held ->
                    Released

                Released ->
                    Inactive

                Inactive ->
                    Inactive
