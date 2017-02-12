module Screens.LevelCreationScreen exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2
import Color
import GameTypes exposing (DeltaTime, Vector, Player, Barrel, ActiveElement(..))
import Coordinates exposing (gameSize)
import GameLogic exposing (calculateActiveElement)
import Player exposing (PlayerControls, updatePlayer, renderPlayer, PlayerControls, initialPlayerControls)
import Barrel exposing (renderBarrel)
import Button exposing (ButtonState(..), calculateButtonState)


type LevelCreationMode
    = PlayTest
    | LevelEdit


type alias LevelCreateState =
    { player : Player
    , barrels : List Barrel
    , activeElement : ActiveElement
    , camera : Camera
    , resources : Resources
    , levelCreationMode : LevelCreationMode
    , editModeButtons : EditModeControls
    , playTestModeButtons : PlayTestModeControls
    }


type alias EditModeControls =
    { switchToPlayTestMode : ButtonState
    }


defaultEditModeButtons : EditModeControls
defaultEditModeButtons =
    { switchToPlayTestMode = Inactive
    }


type alias PlayTestModeControls =
    { switchToEditMode : ButtonState
    , playerControls : PlayerControls
    }


defaultPlayTestModeButtons : PlayTestModeControls
defaultPlayTestModeButtons =
    { switchToEditMode = Inactive
    , playerControls = initialPlayerControls
    }


initialLevelCreateState : LevelCreateState
initialLevelCreateState =
    let
        startingPoint =
            ( 0, 0 )

        ( gameWidth, gameHeight ) =
            gameSize
    in
        { player = Player startingPoint ( 0, 0 ) 45
        , barrels = [ Barrel ( 0, -100 ) (3 * pi / 4) 45, Barrel ( 200, -100 ) (pi / 4) 45 ]
        , activeElement = ThePlayer
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , levelCreationMode = PlayTest
        , editModeButtons = defaultEditModeButtons
        , playTestModeButtons = defaultPlayTestModeButtons
        }


levelCreateScreenUpdate : DeltaTime -> List Vector -> LevelCreateState -> LevelCreateState
levelCreateScreenUpdate deltaTime touchLocations state =
    case state.levelCreationMode of
        PlayTest ->
            updatePlayTestingMode deltaTime touchLocations state

        LevelEdit ->
            updateLevelEditMode deltaTime touchLocations state


updatePlayTestingMode : DeltaTime -> List Vector -> LevelCreateState -> LevelCreateState
updatePlayTestingMode deltaTime touchLocations state =
    let
        buttonsPressed =
            calculatePlayTestButtonsPressed touchLocations state.playTestModeButtons

        activeElement =
            calculateActiveElement state.player state.barrels

        ( newActiveElement, newPlayer ) =
            updatePlayer deltaTime activeElement touchLocations state.playTestModeButtons.playerControls state.player

        levelCreationMode =
            if buttonsPressed.switchToEditMode == Pressed then
                LevelEdit
            else
                PlayTest
    in
        { state
            | player = newPlayer
            , camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
            , activeElement = newActiveElement
            , levelCreationMode = levelCreationMode
        }


calculatePlayTestButtonsPressed : List Vector -> PlayTestModeControls -> PlayTestModeControls
calculatePlayTestButtonsPressed touchLocations playTestControls =
    { playTestControls
        | switchToEditMode = calculateButtonState (List.any (\( x, y ) -> x < 250 && x > 1000) touchLocations) playTestControls.switchToEditMode
    }


updateLevelEditMode : DeltaTime -> List Vector -> LevelCreateState -> LevelCreateState
updateLevelEditMode deltaTime touchLocations state =
    let
        editModeButtonsPressed =
            calculateEditModeButtonsPressed touchLocations state.editModeButtons

        levelCreationMode =
            if editModeButtonsPressed.switchToPlayTestMode == Pressed then
                PlayTest
            else
                LevelEdit
    in
        { state
            | levelCreationMode = levelCreationMode
        }


calculateEditModeButtonsPressed : List Vector -> EditModeControls -> EditModeControls
calculateEditModeButtonsPressed touchLocations editModeControls =
    { switchToPlayTestMode = calculateButtonState (List.any (\( x, y ) -> x < 250 && x > 1000) touchLocations) editModeControls.switchToPlayTestMode
    }


renderLevelCreation : LevelCreateState -> ( Camera, List Renderable )
renderLevelCreation state =
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
