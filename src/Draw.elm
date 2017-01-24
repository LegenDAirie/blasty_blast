module Draw exposing (renderPlayer, renderBarrel, renderTouch)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition, getViewSize)
import Vector2 as V2 exposing (getX, getY)
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


renderTouch : ( Float, Float ) -> Camera -> Renderable
renderTouch location camera =
    Render.rectangle
        { color = Color.darkBlue
        , position = location
        , size = ( toFloat 30, toFloat 30 )
        }
