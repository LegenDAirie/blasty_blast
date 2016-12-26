module GameTypes
    exposing
        ( Barrel
        , Vector
        , DeltaTime
        , Controles(..)
        , ActiveElement(..)
        )


type Controles
    = GoLeft
    | GoRight
    | GoWithTheFlow


type ActiveElement
    = ThePlayer
    | ThisBarrel Barrel


type alias DeltaTime =
    Float


type alias Vector =
    ( Float, Float )


type alias Barrel =
    { location : Vector
    , angle : Float
    , collisionRadius : Int
    }
