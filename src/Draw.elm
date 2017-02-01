module Draw exposing (render, renderPlayer, renderBarrel, renderTouch)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition, getViewSize)
import GameTypes exposing (GameScreen(..), LevelCreateState, Model, Vector, Player, Barrel, LevelCreationMode(..))
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Color
import Coordinates exposing (convertTouchCoorToGameCoor, gameSize)


render : Model -> ( Camera, List Renderable )
render model =
    case model.gameScreen of
        Uninitialized ->
            ( Camera.fixedWidth (getX gameSize) ( 0, 0 ), [] )

        LevelCreateScreen state ->
            let
                overlay =
                    case state.levelCreationMode of
                        PlayTest ->
                            renderPlayTestOverlay state

                        LevelEdit ->
                            renderEditModeOverlay state
            in
                ( state.camera
                , List.concat
                    [ [ renderPlayer state.resources state.player ]
                    , (List.map renderBarrel state.barrels)
                    , List.map (renderTouch state.camera) model.touchLocations
                    , overlay
                    ]
                )


renderPlayTestOverlay : LevelCreateState -> List Renderable
renderPlayTestOverlay state =
    [ renderEditMode state.camera ]


renderEditModeOverlay : LevelCreateState -> List Renderable
renderEditModeOverlay state =
    [ renderAddBarrel state.camera
    , renderEditHudBar state.camera
    , renderPlayTestMode state.camera
    ]


renderEditMode : Camera -> Renderable
renderEditMode camera =
    let
        ( gameUnitWidth, gameUnitHeight ) =
            gameSize

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
            gameSize

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
            gameSize

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
        ( gameUnitWidth, gameUnitHeight ) =
            gameSize

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


renderPlayer : Resources -> Player -> Renderable
renderPlayer resources player =
    -- let
    --     ( x, y ) =
    --         player.location
    -- in
    --     Render.animatedSpriteWithOptions
    --         { position = ( x, y, 0 )
    --         , size = ( toFloat player.collisionRadius * 2, toFloat player.collisionRadius * 2 )
    --         , texture = Resources.getTexture "../assets/ghost-friend.png" resources
    --         , bottomLeft = ( 0, 0 )
    --         , topRight = ( 1, 1 )
    --         , duration = 1
    --         , numberOfFrames = 8
    --         , rotation = 0
    --         , pivot = ( 0.5, 0 )
    --         }
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


renderTouch : Camera -> Vector -> Renderable
renderTouch camera location =
    Render.rectangle
        { color = Color.darkBlue
        , position = convertTouchCoorToGameCoor camera location
        , size = ( toFloat 30, toFloat 30 )
        }
