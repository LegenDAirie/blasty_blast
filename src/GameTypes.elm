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
import Game.Resources as Resources exposing (Resources)
import Touch exposing (TouchEvent(..), Touch)


type alias Model =
    { canvasSize : Vector
    , player : Player
    , barrels : List Barrel
    , active : ActiveElement
    , force : Force
    , camera : Camera
    , resources : Resources
    , touchLocations : List Vector
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
