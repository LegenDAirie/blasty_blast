module GameTypes
    exposing
        ( Barrel
        , Player
        , Vector
        , DeltaTime
        , ActiveElement(..)
        )


type alias Player =
    { location : Vector
    , velocity : Vector
    , collisionRadius : Int
    }


type alias Barrel =
    { location : Vector
    , angle : Float
    , collisionRadius : Int
    }


type ActiveElement
    = ThePlayer
    | ThisBarrel Barrel


type alias Vector =
    ( Float, Float )


type alias DeltaTime =
    Float
