module GameTypes
    exposing
        ( Model
        , Barrel
        , Player
        , Vector
        , DeltaTime
        , GameScreen(..)
        , LevelCreationMode(..)
        , LevelCreateState
        , ActiveElement(..)
        )

import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)
import Game.Resources as Resources exposing (Resources)


type alias Model =
    { canvasSize : Vector
    , touchLocations : List Vector
    , gameScreen : GameScreen
    }


type GameScreen
    = Uninitialized
    | LevelCreateScreen LevelCreateState


type alias LevelCreateState =
    { player : Player
    , barrels : List Barrel
    , activeElement : ActiveElement
    , camera : Camera
    , resources : Resources
    , levelCreationMode : LevelCreationMode
    , debug : String
    }


type LevelCreationMode
    = PlayTest
    | LevelEdit


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
