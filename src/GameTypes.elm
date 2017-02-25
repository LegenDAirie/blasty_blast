module GameTypes exposing (..)

import Button exposing (ButtonState)


type alias Vector =
    ( Float, Float )


type alias DeltaTime =
    Float


type alias Player =
    { location : Vector
    , velocity : Vector
    , collisionRadius : Int
    }


type alias PlayerControls =
    { left : ButtonState
    , right : ButtonState
    , fire : ButtonState
    }


type ActiveElement
    = ThePlayer
    | ThisBarrel Barrel



----------------------------------------------------------
----------------------------------------------------------


type alias Barrel =
    { location : Vector
    , angle : Float
    , collisionRadius : Int
    , rotation : Rotation
    , movement : Movement
    }


type Movement
    = NoMovement
    | LinePath LineMovementSpec
    | CirclePath CircleMovementSpec


type alias LineMovementSpec =
    { startNode : Vector
    , endNode : Vector
    , startingDirection : StartingDirection
    , speed : Float
    }


type StartingDirection
    = LeftOrDown
    | RightOrUp


type alias CircleMovementSpec =
    { trackRadius : Float
    , clockWiseMovement : Bool
    }


type Rotation
    = NoRotation NoRotationSpec
    | AutoWithNoControl AutoWithNoControlSpec
    | AutoWithDirectionControl AutoWithDirectionControlSpec
    | AutoRotateToAndStop AutoRotateToAndStopSpec
    | ManualRotation ManualRotationSpec
    | ManualTimedFire ManualTimedFireSpec


type alias NoRotationSpec =
    { autoFire : Bool
    }


type alias AutoWithNoControlSpec =
    { range : Vector
    , clockWise : Bool
    , rotationStyle : RotationStyle
    }


type alias AutoWithDirectionControlSpec =
    { clockWise : Bool
    }


type alias AutoRotateToAndStopSpec =
    { autoFire : Bool
    , endAngle : Vector
    }


type alias ManualRotationSpec =
    { range : Vector
    }


type alias ManualTimedFireSpec =
    { timeTillFire : Float
    }


type RotationStyle
    = Continuous
    | InSteps



----------------------------------------------------------
----------------------------------------------------------
