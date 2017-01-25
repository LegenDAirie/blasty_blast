module Draw exposing (renderPlayer, renderBarrel, renderTouch)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition, getViewSize)
import GameTypes exposing (Player, Barrel)
import Vector2 as V2 exposing (getX, getY)
import Color


renderPlayer : Player -> Renderable
renderPlayer player =
    Render.rectangle
        { color = Color.charcoal
        , position = player.location
        , size = ( toFloat player.collisionRadius * 2, toFloat player.collisionRadius * 2 )
        }


renderBarrel : Barrel -> Renderable
renderBarrel barrel =
    Render.rectangleWithOptions
        { color = Color.brown
        , position = ( getX barrel.location, getY barrel.location, 0 )
        , rotation = barrel.angle
        , size = ( toFloat barrel.collisionRadius * 2, toFloat barrel.collisionRadius * 2 )
        , pivot = ( 0.5, 0.5 )
        }


renderTouch : ( Float, Float ) -> Camera -> Renderable
renderTouch location camera =
    Render.rectangle
        { color = Color.darkBlue
        , position = location
        , size = ( toFloat 30, toFloat 30 )
        }
