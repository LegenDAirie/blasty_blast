module Screens.LevelCreationScreen exposing (..)

import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera, getPosition)
import Game.Resources as Resources exposing (Resources)
import Vector2 as V2 exposing (getX, getY)
import Color
import GameTypes exposing (DeltaTime, Vector, Player, Barrel, ActiveElement(..))
import Coordinates exposing (gameSize, convertTouchCoorToGameCoor)
import Player exposing (PlayerControls, updatePlayer, renderPlayer, PlayerControls, initialPlayerControls, calculatePlayerButtonsPressed)
import Barrel exposing (renderBarrel)
import Button exposing (ButtonState(..), calculateButtonState)
import GameLogic
    exposing
        ( calculateActivePlayElement
        , repostionBarrels
        , removeOverlappingBarrels
        , touchIsCollidingWithBarrel
        , areAnyBarrelsInTheWay
        )


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
    , playerControls : PlayerControls
    , scrollScreenButton : ButtonState
    , scrollScreenStartLocation : Maybe Vector
    }


type alias EditModeControls =
    { switchToPlayTestMode : ButtonState
    , addBarrelButton : ButtonState
    }


defaultEditModeButtons : EditModeControls
defaultEditModeButtons =
    { switchToPlayTestMode = Inactive
    , addBarrelButton = Inactive
    }


type alias PlayTestModeControls =
    { switchToEditMode : ButtonState
    }


defaultPlayTestModeButtons : PlayTestModeControls
defaultPlayTestModeButtons =
    { switchToEditMode = Inactive
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
        , barrels = [ Barrel ( 0, -100 ) (pi / 2) 45, Barrel ( 200, -100 ) (pi / 2) 45 ]
        , activeElement = ThePlayer
        , camera = Camera.fixedWidth gameWidth startingPoint
        , resources = Resources.init
        , levelCreationMode = PlayTest
        , editModeButtons = defaultEditModeButtons
        , playTestModeButtons = defaultPlayTestModeButtons
        , playerControls = initialPlayerControls
        , scrollScreenButton = Inactive
        , scrollScreenStartLocation = Nothing
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
        playTestModebuttonsPressed =
            calculatePlayTestButtonsPressed touchLocations state.playTestModeButtons

        playerButtonsPressed =
            calculatePlayerButtonsPressed touchLocations state.playerControls

        activeElement =
            calculateActivePlayElement state.player state.barrels

        newActiveElement =
            if playerButtonsPressed.fire == Pressed then
                ThePlayer
            else
                activeElement

        newPlayer =
            updatePlayer deltaTime activeElement state.playerControls state.player

        levelCreationMode =
            if playTestModebuttonsPressed.switchToEditMode == Pressed then
                LevelEdit
            else
                PlayTest
    in
        { state
            | player = newPlayer
            , camera = Camera.follow 0.5 0.17 (V2.sub state.player.location ( -100, -100 )) state.camera
            , activeElement = newActiveElement
            , levelCreationMode = levelCreationMode
            , playTestModeButtons = playTestModebuttonsPressed
            , playerControls = playerButtonsPressed
        }


calculatePlayTestButtonsPressed : List Vector -> PlayTestModeControls -> PlayTestModeControls
calculatePlayTestButtonsPressed touchLocations playTestControls =
    let
        switchToEditModePressed =
            touchLocations
                |> List.any (\( x, y ) -> x < 250 && y > 500)
    in
        { playTestControls
            | switchToEditMode = calculateButtonState switchToEditModePressed playTestControls.switchToEditMode
        }


updateLevelEditMode : DeltaTime -> List Vector -> LevelCreateState -> LevelCreateState
updateLevelEditMode deltaTime touchLocations state =
    let
        editModeButtonsPressed =
            calculateEditModeButtonsPressed touchLocations state.editModeButtons

        touchesInGameCoordinates =
            touchLocations
                |> List.map (convertTouchCoorToGameCoor state.camera)

        beingScrolled =
            isScreenBeingScrolled touchesInGameCoordinates state.barrels state.editModeButtons

        ( offSetCameraBy, newStartLocation ) =
            if beingScrolled then
                state.scrollScreenStartLocation
                    |> getScreenScrollCameraOffSet (List.head touchLocations)
            else
                ( ( 0, 0 ), Nothing )

        newCamera =
            Camera.moveBy offSetCameraBy state.camera

        newBarrels =
            state.barrels
                |> addBarrel editModeButtonsPressed.addBarrelButton state.camera
                |> repostionBarrels touchesInGameCoordinates
                |> removeOverlappingBarrels

        levelCreationMode =
            if editModeButtonsPressed.switchToPlayTestMode == Pressed then
                PlayTest
            else
                LevelEdit
    in
        { state
            | levelCreationMode = levelCreationMode
            , editModeButtons = editModeButtonsPressed
            , barrels = newBarrels
            , camera = newCamera
            , scrollScreenStartLocation = newStartLocation
        }


getScreenScrollCameraOffSet : Maybe Vector -> Maybe Vector -> ( Vector, Maybe Vector )
getScreenScrollCameraOffSet currentLocation startLocation =
    case startLocation of
        Just start ->
            case currentLocation of
                Just end ->
                    let
                        dif =
                            V2.sub start end
                                |> (\( x, y ) -> ( x, -y ))
                    in
                        ( dif, Just end )

                Nothing ->
                    ( ( 0, 0 ), Nothing )

        Nothing ->
            case currentLocation of
                Just end ->
                    ( ( 0, 0 ), Just end )

                Nothing ->
                    ( ( 0, 0 ), Nothing )


isScreenBeingScrolled : List Vector -> List Barrel -> EditModeControls -> Bool
isScreenBeingScrolled touchLocations barrels editModeControls =
    let
        barrelsInTheWay =
            areAnyBarrelsInTheWay touchLocations barrels

        allButtonsInactive =
            [ editModeControls.addBarrelButton
            , editModeControls.switchToPlayTestMode
            ]
                |> List.all (\buttonState -> buttonState == Inactive)
    in
        not barrelsInTheWay && allButtonsInactive


calculateEditModeButtonsPressed : List Vector -> EditModeControls -> EditModeControls
calculateEditModeButtonsPressed touchLocations editModeControls =
    let
        isSwitchToPlayTestModePressed =
            touchLocations
                |> List.any (\( x, y ) -> x < 250 && y > 500)

        isAddBarrelPressed =
            touchLocations
                |> List.any (\( x, y ) -> x > 960 && y < 200)
    in
        { switchToPlayTestMode = calculateButtonState isSwitchToPlayTestModePressed editModeControls.switchToPlayTestMode
        , addBarrelButton = calculateButtonState isAddBarrelPressed editModeControls.addBarrelButton
        }


addBarrel : ButtonState -> Camera -> List Barrel -> List Barrel
addBarrel buttonPressed camera barrels =
    case buttonPressed of
        Pressed ->
            let
                newBarrelLocation =
                    ( 1120 + 45, 100 - 45 )
                        |> convertTouchCoorToGameCoor camera

                newBarrel =
                    Barrel newBarrelLocation 0 45
            in
                List.append barrels [ newBarrel ]

        _ ->
            barrels


renderLevelCreation : LevelCreateState -> List Renderable
renderLevelCreation state =
    let
        overlay =
            case state.levelCreationMode of
                PlayTest ->
                    renderPlayTestOverlay state

                LevelEdit ->
                    renderEditModeOverlay state
    in
        List.concat
            [ [ renderPlayer state.resources state.player ]
            , (List.map renderBarrel state.barrels)
            , overlay
            ]


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
