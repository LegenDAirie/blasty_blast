module Draw exposing (render, renderPlayer, renderBarrel, renderTouch)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition, getViewSize)
import GameTypes exposing (Model, Vector, Player, Barrel, CreateMode(..))
import Vector2 as V2 exposing (getX, getY)
import Color
import Window


render : Model -> List Renderable
render model =
    let
        overlay =
            case model.mode of
                PlayTest ->
                    renderPlayTestOverlay model

                Edit ->
                    renderEditModeOverlay model
    in
        List.concat
            [ [ renderPlayer model.player ]
            , (List.map renderBarrel model.barrels)
            , [ renderTouch model.touchLocation model.camera ]
            , overlay
            ]


renderPlayTestOverlay : Model -> List Renderable
renderPlayTestOverlay model =
    [ renderEditMode model.camera ]


renderEditModeOverlay : Model -> List Renderable
renderEditModeOverlay model =
    [ renderAddBarrel model.camera
    , renderEditHudBar model.camera
    , renderPlayTestMode model.camera
    ]


renderEditMode : Camera -> Renderable
renderEditMode camera =
    let
        ( gameUnitWidth, gameUnitHeight ) =
            ( 1280, 720 )

        ( editModeWidth, editModeHeight ) =
            ( gameUnitWidth / 8, gameUnitHeight / 5 )

        ( xOffset, yOffset ) =
            ( gameUnitWidth / 2, gameUnitHeight / 2 )

        ( x, y ) =
            getPosition camera

        editModePosition =
            ( x - xOffset, y - yOffset )
    in
        Render.rectangle
            { color = Color.orange
            , position = editModePosition
            , size = ( editModeWidth, editModeHeight )
            }


renderPlayTestMode : Camera -> Renderable
renderPlayTestMode camera =
    let
        ( gameUnitWidth, gameUnitHeight ) =
            ( 1280, 720 )

        ( playTestWidth, playTestHeight ) =
            ( gameUnitWidth / 8, gameUnitHeight / 5 )

        ( xOffset, yOffset ) =
            ( gameUnitWidth / 2, gameUnitHeight / 2 )

        ( x, y ) =
            getPosition camera

        playTestPosition =
            ( x - xOffset, y - yOffset )
    in
        Render.rectangle
            { color = Color.orange
            , position = playTestPosition
            , size = ( playTestWidth, playTestHeight )
            }


renderAddBarrel : Camera -> Renderable
renderAddBarrel camera =
    let
        ( gameUnitWidth, gameUnitHeight ) =
            ( 1280, 720 )

        ( addBarrelWidth, addBarrelHeight ) =
            ( gameUnitWidth / 8, gameUnitHeight / 5 )

        ( xOffset, yOffset ) =
            ( gameUnitWidth / 2 - addBarrelWidth, gameUnitHeight / 2 - addBarrelHeight )

        ( x, y ) =
            getPosition camera

        addBarrelPosition =
            ( x + xOffset, y + yOffset )
    in
        Render.rectangle
            { color = Color.purple
            , position = addBarrelPosition
            , size = ( addBarrelWidth, addBarrelHeight )
            }


renderEditHudBar : Camera -> Renderable
renderEditHudBar camera =
    let
        -- not sure where game resolution should live
        ( gameUnitWidth, gameUnitHeight ) =
            ( 1280, 720 )

        ( selectionBarWidth, selectionBarHeight ) =
            ( gameUnitWidth, gameUnitHeight / 5 )

        ( xOffset, yOffset ) =
            ( gameUnitWidth / 2, gameUnitHeight / 2 - selectionBarHeight )

        ( x, y ) =
            getPosition camera

        hudPosition =
            ( x - xOffset, y + yOffset )
    in
        Render.rectangle
            { color = Color.darkCharcoal
            , position = hudPosition
            , size = ( selectionBarWidth, selectionBarHeight )
            }


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
