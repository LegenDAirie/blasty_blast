module GameTypes
    exposing
        ( Model
        , Barrel
        , Player
        , Vector
        , DeltaTime
        , GameScreens(..)
        , ActiveElement(..)
        )

import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)
import Game.Resources as Resources exposing (Resources)


type alias Model =
    { canvasSize : Vector
    , player : Player
    , barrels : List Barrel
    , camera : Camera
    , resources : Resources
    , touchLocations : List Vector
    , gameScreen : GameScreens
    , debug : String
    }


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


type GameScreens
    = PlayTest
    | LevelEdit


type ActiveElement
    = ThePlayer
    | ThisBarrel Barrel


type alias Vector =
    ( Float, Float )


type alias DeltaTime =
    Float
