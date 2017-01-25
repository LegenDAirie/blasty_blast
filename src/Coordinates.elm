module Coordinates exposing (convertTouchCoorToGameCoor, convertToGameUnits)

import GameTypes exposing (Vector)
import Vector2 as V2 exposing (distance, normalize, setX, getX, getY)
import Game.TwoD.Camera as Camera exposing (Camera, getViewSize, getPosition)


convertTouchCoorToGameCoor : Vector -> Float -> Camera -> Vector -> Vector
convertTouchCoorToGameCoor gameSize sizeRatio camera touchLocation =
    touchLocation
        |> convertToGameUnits sizeRatio
        |> offSetOrigin gameSize
        |> offSetByCamera camera
        |> flipY


flipY : Vector -> Vector
flipY ( x, y ) =
    ( x, -y )


convertToGameUnits : Float -> Vector -> Vector
convertToGameUnits sizeRatio touchLocation =
    V2.scale sizeRatio touchLocation


offSetOrigin : Vector -> Vector -> Vector
offSetOrigin gameSize touchLocation =
    gameSize
        |> V2.scale 0.5
        |> V2.sub touchLocation


offSetByCamera : Camera -> Vector -> Vector
offSetByCamera camera touchLocation =
    camera
        |> getPosition
        |> flipY
        |> V2.add touchLocation
