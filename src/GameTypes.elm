module GameTypes
    exposing
        ( Barrel
        , Vector
        , Force(..)
        , Controles(..)
        , ActiveElement(..)
        )


type Force
    = GoLeft
    | GoRight
    | GoWithTheFlow


type Controles
    = Left
    | Right
    | Fire
    | None


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
