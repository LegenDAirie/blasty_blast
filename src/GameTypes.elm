module GameTypes
    exposing
        ( Model
        , Barrel
        , Player
        , Vector
        , DeltaTime
        , CreateMode(..)
        , Force(..)
        , PlayTestControles(..)
        , ActiveElement(..)
        )

import Window
import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)


type alias Model =
    { windowSize : Window.Size
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , force : Force
    , camera : Camera
    , touchLocation : Vector
    , mode : CreateMode
    , debug : String
    }


type CreateMode
    = PlayTest
    | Edit


type Force
    = GoLeft
    | GoRight
    | GoWithTheFlow



-- play controles
-- type Controles
--     = Left
--     | Right
--     | Fire
--     | None
-- playtest controles


type PlayTestControles
    = Left
    | Right
    | Fire
      -- | SwitchToEditMode
    | None


type ActiveElement
    = ThePlayer
    | ThisBarrel Barrel


type alias Vector =
    ( Float, Float )


type alias DeltaTime =
    Float


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
