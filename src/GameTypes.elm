module GameTypes
    exposing
        ( Barrel
        , Vector
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


type alias Vector =
    ( Float, Float )


type alias Barrel =
    { location : Vector
    , angle : Float
    , collisionRadius : Int
    }
