module Draw exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Color
import Player exposing (Player)
import Barrel exposing (Barrel)


renderPlayer : Player -> Renderable
renderPlayer player =
    Render.rectangle
        { color = Color.charcoal
        , position = player.location
        , size = ( toFloat player.collisionRadius, toFloat player.collisionRadius )
        }


renderBarrel : Barrel -> Renderable
renderBarrel barrel =
    Render.rectangle
        { color = Color.brown
        , position = barrel.location
        , size = ( toFloat barrel.collisionRadius, toFloat barrel.collisionRadius )
        }
